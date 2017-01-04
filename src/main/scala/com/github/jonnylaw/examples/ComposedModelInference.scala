package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import scala.concurrent.ExecutionContext.Implicits.global
import akka.NotUsed
import akka.stream.scaladsl._
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.util.ByteString

import com.github.jonnylaw.model._
import State._
import Parameters._
import StateSpace._

import java.nio.file.Paths
import breeze.stats.distributions._
import breeze.linalg.{DenseVector, diag}
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
    Some(1.0),
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
  implicit val system = ActorSystem("SimulateSeasonalPoisson")
  implicit val materializer = ActorMaterializer()

  val times = (1 to 100).map(_.toDouble).toList

  val res = model(params) map {mod =>
    Source(times).
      via(mod.simPompModel(0.0)).
      map(s => ByteString(s + "\n")).
      runWith(FileIO.toPath(Paths.get("seasonalPoissonSims.csv")))
  }

  res.right.map(_.onComplete(_ => system.terminate))
}

object FilterSeasonalPoisson extends App with TestModel {
  implicit val system = ActorSystem("FilterPoisson")
  implicit val materializer = ActorMaterializer()

  // number of particles for the particle filter
  val n = 1000

  val res = for {
    mod <- model(params)
    observations = mod.simRegular(0.1)
    bootstrapFilter = Filter(mod, ParticleFilter.multinomialResampling)
  } yield observations.
    take(100).
    via(bootstrapFilter.filter(0.0)(n)).
    drop(1).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toPath(Paths.get("OnlineComposedModelFiltered.csv")))

  res.right.map(_.onComplete(_ => system.shutdown))
}

object DetermineComposedParams extends App with TestModel {
  implicit val system = ActorSystem("DeterminePoissonParams")
  implicit val materializer = ActorMaterializer()

  val data = FileIO.fromPath(Paths.get("seasonalPoissonSims.csv")).
    via(Framing.delimiter(
      ByteString("\n"),
      maximumFrameLength = 256,
      allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

  data.
    take(100).
    grouped(100).
    map(d => {

      def mll(p: Parameters) = {
        for {
          mod <- model(p)
          filter = Filter(mod, ParticleFilter.multinomialResampling)
        } yield filter.llFilter(d.toVector, d.map(_.t).min)(200)
      }

      MetropolisParams(mll, Parameters.perturb(0.05), params).
        iters.
        take(1000).
        map(s => ByteString(s + "\n")).
        runWith(FileIO.toPath(Paths.get("SeasonalPoissonParams.csv"))).
        onComplete(_ => system.terminate)

    }).
    runWith(Sink.ignore)
}
