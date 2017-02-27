package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats._
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import java.io._
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait PoissonTestModel {
  val poissonParam: Parameters = Parameters.leafParameter(
    None,
    SdeParameter.brownianParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      mu = DenseVector(0.1),
      sigma = DenseVector(0.5))
  )

  val mod = Model.poissonModel(Sde.brownianMotion)

  val prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(None, BrownianParameter(m, s, mu, sigma)) =>
      Gaussian(0.5, 3.0).logPdf(m(0)) +
      Gamma(1.0, 1.0).logPdf(s(0)) + 
      Gaussian(0.5, 3.0).logPdf(mu(0)) +
      Gamma(1.0, 2.0).logPdf(sigma(0))
  }

  implicit val system = ActorSystem("PoissonModel")
  implicit val materializer = ActorMaterializer()
}

object SimPoissonModel extends App with PoissonTestModel {
  SimulateData(mod(poissonParam)).
    observations.
    take(500).
    map((d: Data) => d.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/PoissonModelSims.csv"))).
    onComplete(_ => system.terminate())
}

/**
  * Determine how many particles are required to run the MCMC
  */
object PilotRunPoisson extends App with PoissonTestModel {
  val dataStream = FileIO.fromPath(Paths.get("data/PoissonModelSims.csv")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 256, allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    take(400).
    runWith(Sink.seq)

  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[Vector, Id, State] = Resampling.treeSystematicResampling _

  val res = for {
    data <- dataStream
    out = Streaming.pilotRun[Vector](data.toVector, mod, poissonParam, resample, particles)
    io <- out.map { case (n, v) => s"$n, $v\n" }.
      runWith(Streaming.writeStreamToFile("data/PoissonPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

/**
  * Use the first 400 simulations to determine the full-joint posterior of the poisson model
  */
object DeterminePoissonPosterior extends App with PoissonTestModel {
  setParallelismGlobally(12)

  // read in the data
  val trainingData = FileIO.fromPath(Paths.get("data/PoissonModelSims.csv")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 256, allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    take(400).
    runWith(Sink.seq)

  // Choose n based on the pilot run, a rule of thumb is the variance of the log of the
  // estimated marginal likelihood is equal to one
  val n = 200

  // serialize the output, however we are required to retain the output of the 
  // stream in memory, could use scodec for this
  val res = for {
    data <- trainingData
    pf = ParticleFilter.filterLlState[Vector](data.toVector, Resampling.treeSystematicResampling, n).compose(mod)
    pmmh = ParticleMetropolisState(pf.run, poissonParam, Parameters.perturb(0.025), prior)
    iters <- Source.fromIterator(() => pmmh.iters.steps).take(10000).runWith(Sink.seq)
  } yield Streaming.serialiseToFile(iters, "data/PoissonParams")

  res.onComplete(_ => system.terminate())
}

/**
  * Run the filter over the last 100 elements of the simulted data using 
  * samples from the joint-posterior of the state and parameters, p(x, theta | y) 
  */
object FilterPoisson extends App with PoissonTestModel {
  val testData = FileIO.fromPath(Paths.get("data/PoissonModelSims.csv")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 256, allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    drop(400).
    take(100)

  // the time of the draw from the joint posterior p(x, theta | y)
  val t0 = 400.0

  // pattern match on the deserialised file
  val its: Vector[MetropState] = (Streaming.deserialiseFile("data/PoissonParams"): @unchecked) match {
    case p: Vector[MetropState @unchecked] => p
  }

  // calculate the mean of the parameters
  val paramsState = Resampling.sampleMany(100, its).
    map(s => (s.params, s.state.last)).toVector

  val resample: Resample[Vector, Id, (Parameters, State)] = Resampling.treeSystematicResampling

  val filter = ParticleFilter.filterFromPosterior[Vector](resample, paramsState, t0)(mod.run)

  testData.
    via(filter).
    map(s => PfState(s.t, s.observation, s.ps.map(_._2), s.ll, 0)). 
    map(ParticleFilter.getIntervals(mod(poissonParam))).
    map(_.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/PoissonModelFiltered.csv"))).
    onComplete(_ => system.terminate())
}

/**
  * Perform a one step forecast on the poisson data, using unseen test data,
  * Sampling from the joint posterior of the parameters and the state p(x, theta | y)
  */
object OneStepForecastPoisson extends App with PoissonTestModel {
  val testData = FileIO.fromPath(Paths.get("data/PoissonModelSims.csv")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 256, allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    drop(400).
    take(100)

  // pattern match on the deserialised file
  val its: Vector[MetropState] = (Streaming.deserialiseFile("data/PoissonParams"): @unchecked) match {
    case p: Vector[MetropState @unchecked] => p
  }

  val t0 = 400.0

  // Sample from the vector
  val paramsState = Resampling.sampleMany(100, its).
    map(s => (s.params, s.state.last)).
    toVector

  val resample: Resample[Vector, Id, (Parameters, State)] = Resampling.treeSystematicResampling

  val filter = ParticleFilter.filterFromPosterior[Vector](resample, paramsState, t0)(mod.run)

  testData.
    via(filter).
    map(s => PfState(s.t, s.observation, s.ps.map(_._2), s.ll, 0)). 
    map(s => ParticleFilter.getMeanForecast(s, mod(poissonParam), s.t + 0.1)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/PoissonModelForecast.csv")).
    onComplete(_ => system.terminate())
}

/**
  * Perform a long term forecast, by sampling from the full joint posterior p(x, theta | y)
  * a pair consisting of the state at the time of the last observation and the associated parameter*value
  */
object LongTermForecast extends App with PoissonTestModel {
  println("Deserialising File")

  // pattern match on the deserialised file
  val its: Vector[MetropState] = (Streaming.deserialiseFile("data/PoissonParams"): @unchecked) match {
    case p: Vector[MetropState @unchecked] => p
  }

  println("File Deserialised")

  println("Sampling from parameter dist")
  // Sample from the vector
  val paramsState = Resampling.sampleMany(10, its).
    map(s => (s.params, s.state.last))

  println(s"Parameter Dist Sample: ${paramsState.mkString(", ")}")

  // the times we wish to forecast
  // a single forecast
  def forecast(p: Parameters, s: State) = 
    Source.apply(401.0 to 500.0 by 1.0).
    via(SimulateData(mod(p)).simPompModel(401.0)).
    runWith(Sink.seq)

  println("Starting parallel forecast for 100 time points")
  // construct a parallel task
  Source.apply(paramsState).
    mapAsyncUnordered(8){ case (p, s) =>
      forecast(p, s) map (f => (p, f)) 
    }.
    map { case (p, f) => ParticleFilter.summariseForecast(mod(p), 0.5)(f.toVector) }.
    map(_.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/PoissonLongForecast.csv"))).
    onComplete(_ => system.terminate())
}
