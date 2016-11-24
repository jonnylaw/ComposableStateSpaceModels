package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import scala.concurrent.ExecutionContext.Implicits.global
import akka.NotUsed
import akka.stream.scaladsl.Source
import akka.stream.scaladsl._
import scala.concurrent.{duration, Await}
import scala.concurrent.duration._
import akka.util.ByteString

import com.github.jonnylaw.model._
import UnparamModel._
import Streaming._
import DataTypes._
import SimData._
import Utilities._
import State._
import Parameters._
import StateSpace._
import java.nio.file.Paths

import breeze.stats.distributions.Gaussian
import breeze.linalg.{DenseVector, diag}
import scala.concurrent.duration._
import breeze.numerics.exp
import cats.implicits._

/**
  * Define a model to use throughout the examples in this file
  */
trait TestModel {
  val poissonParams: Parameters = LeafParameter(
    GaussianParameter(0.0, 0.5),
    None,
    BrownianParameter(-0.05, 0.3))
  val seasonalParams: Parameters = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.0),
      diag(DenseVector.fill(6)(0.5))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(1.0), DenseVector.fill(6)(0.1), DenseVector.fill(6)(0.5)))

  val params = poissonParams |+| seasonalParams
  val poisson: UnparamModel = PoissonModel(stepBrownian)
  val seasonal: UnparamModel = SeasonalModel(24, 3, stepOrnstein)
  val model = poisson |+| seasonal
}

/**
  * Simulate a poisson model, with seasonal rate parameter
  */
object SimulateSeasonalPoisson extends App with TestModel {
  val times = (1 to 100).map(_.toDouble).toList

  implicit val system = ActorSystem("SimulateSeasonalPoisson")
  implicit val materializer = ActorMaterializer()

  Source(times).
    via(model(params).simPompModel(0.0)).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("seasonalPoissonSims.csv"))).
    onComplete(_ => system.terminate)
}

object DetermineComposedParams extends App with TestModel {
  implicit val system = ActorSystem("DeterminePoissonParams")
  implicit val materializer = ActorMaterializer()

  val data = FileIO.fromPath(Paths.get("SeasonalPoissonSims.csv")).
    via(Framing.delimiter(
      ByteString("\n"),
      maximumFrameLength = 256,
      allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

  val filter = Filter(model, ParticleFilter.multinomialResampling)


  data.
    take(100).
    grouped(100).
    map(d => {
      val mll = filter.llFilter(d.toVector, d.map(_.t).min)(200) _
      ParticleMetropolis(mll, params, Parameters.perturb(0.05)).
        iters.
        take(10000).
        map(s => ByteString(s + "\n")).
        runWith(FileIO.toPath(Paths.get("LinearModelBreezeSingleSite.csv"))).
        onComplete(_ => system.terminate)
    }).
    runWith(Sink.ignore)

}

object FilterOnline extends App with TestModel {
  implicit val system = ActorSystem("StreamingPoisson")
  implicit val materializer = ActorMaterializer()

  // we can simulate from the process as an Akka stream
  // 0.1 is the time increment between observations
  val observations: Source[Data, NotUsed] = model(params).simRegular(0.1)

  // write the observations to a file
  observations.
    take(100).
    map(a => ByteString(s"$a\m")).
    runWith(FileIO.toPath(Paths.get("OnlineComposedModel.csv")))

  // particles and initial state for particle filter
  val n = 1000
  val bootstrapFilter = Filter(model, ParticleFilter.multinomialResampling)

  // run the bootstrap particle filter
  observations.
    via(bootstrapFilter.filter(0.0)(n)(params)).
    drop(1).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toPath(Paths.get("OnlineComposedModelFiltered.csv"))).
    onComplete(_ => system.shutdown)
}
