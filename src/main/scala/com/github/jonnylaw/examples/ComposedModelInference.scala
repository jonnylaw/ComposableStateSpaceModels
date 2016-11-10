package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
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
import java.io.{PrintWriter, File}
import breeze.stats.distributions.Gaussian
import breeze.linalg.{DenseVector, diag}
import scala.concurrent.ExecutionContext.Implicits.global
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
  val sims = simData(times, model(params))

  val pw = new PrintWriter("seasonalPoissonSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

/**
  * Filter the simulated seasonal poisson data in a batch
  */
object FilteringSeasonalPoisson extends App with TestModel {

  // read the data from file and parse it into a Data class 
  val data = scala.io.Source.fromFile("seasonalPoissonSims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs(0).toDouble, rs(1).toDouble, None, None, None)).
    toVector

  // Define the particle filter
  val filter = Filter(model, ParticleFilter.multinomialResampling)

  // Run the particle filter over the observed data using 1,000 particles
  val filtered = filter.filterWithIntervals(data, data.map(_.t).min)(1000)(params)

  val pw = new PrintWriter("seasonalPoissonFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object DetermineComposedParams extends App with TestModel {
  implicit val system = ActorSystem("DeterminePoissonParams")
  implicit val materializer = ActorMaterializer()

  val data = scala.io.Source.fromFile("seasonalPoissonSims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs(0).toDouble, rs(1).toDouble, None, None, None)).
    toVector

  val filter = Filter(model, ParticleFilter.multinomialResampling)
  val mll = filter.llFilter(data, data.map(_.t).min)(200) _

  val iters = ParticleMetropolis(mll, params, Parameters.perturb(0.05)).iters

  val pw = new PrintWriter("SeasonalPoissonParamsLol.csv")
  pw.write(iters.sample(10000).mkString("\n"))
  pw.close()
}

object FilterOnline extends App with TestModel {
  implicit val system = ActorSystem("StreamingPoisson")
  implicit val materializer = ActorMaterializer()

  // we can simulate from the process as a stream
  val initialObservation = simStep(model(params).x0.draw, 0.0, 1.0, model(params))
  val dt = 0.1 // specify the time step of the process

  // unfold holds onto the first item in the Some Tuple and uses it in the next application of the anonymous function
  val observations = Source.unfold(initialObservation){d =>
    Some((simStep(d.sdeState.get, d.t + dt, dt, model(params)), d))
  }.
    take(100)

  // write the observations to a file
  observations.
    map(a => ByteString(s"$a\m")).
    runWith(FileIO.toFile(new File("OnlineComposedModel.csv")))

  // particles and initial state for particle filter
  val n = 1000
  val bootstrapFilter = Filter(model, ParticleFilter.multinomialResampling)

  observations.via(bootstrapFilter.filter(0.0)(n)(params)).
    drop(1).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("OnlineComposedModelFiltered.csv")))

  Thread.sleep(25000) // sleep for 25 seconds

  system.shutdown
}
