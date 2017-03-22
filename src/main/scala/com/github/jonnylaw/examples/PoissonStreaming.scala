package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.util.ByteString
import GraphDSL.Implicits._
import breeze.stats.distributions.Rand
import cats._
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Files;
import java.nio.file.Paths;
import scala.io.StdIn
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._
import scala.util.{Try, Success, Failure}

/**
  * Sample from the joint posterior of the state and parameters p(x, theta | y)
  * Serialize this to JSON using Akka HTTP
  * Write as invalid JSON, by converting each element of the sequence to JSON and writing them on a new line of the output file
  * This is the same as Twitters streaming API
  */
object DeterminePoissonPosterior extends App with PoissonTestModel with DataProtocols {
  val res = for {
    data <- DataFromFile("data/PoissonModelSims.csv").
      observations.
      take(400).
      runWith(Sink.seq)
    pf = (p: Parameters) => ParticleFilter.filterLlState(
      data.toVector,
      Resampling.systematicResampling,
      150)(mod(p))
    pmmh = ParticleMetropolisState(pf, simPrior.draw, Parameters.perturb(0.05), prior)
    iters <- pmmh.
      iters.
      take(10000).
      map(_.toJson).
      map(_.toString).
      runWith(Streaming.writeStreamToFile("data/PoissonPosterior.json"))
    } yield iters

  res.
    onComplete(_ => system.terminate())
}

/**
  * Perform a long term forecast, by sampling from the full joint posterior p(x, theta | y)
  * a pair consisting of the state at the time of the last observation and the associated parameter*value
  */
object LongTermForecastPoisson extends App with PoissonTestModel with DataProtocols {
  // read in the parameter posterior from a JSON file
  val readPosterior: Future[Seq[MetropState]] = FileIO.fromPath(Paths.get("data/PoissonPosterior.json")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
    map(_.utf8String).
    map(_.parseJson.convertTo[MetropState]).
    runWith(Sink.seq)

  // create a distribution over the posterior distribution of the state and parameters
  def createDist[A, B](s: Seq[A])(f: A => B): Rand[B] = new Rand[B] {
    def draw = {
      f(Resampling.sampleOne(s.toVector))
    }
  }

  val t0 = 40.0

  val times = Source(401.0 to 500.0 by 1.0)

  val res = for {
    posterior <- readPosterior
    simPosterior = createDist(posterior)(s => (s.params, s.sde.state))
    io <- times.
      via(SimulateData.forecast(mod.run, t0)(simPosterior)).
      map(_.sample(100).toVector).
      map(SimulateData.summariseForecast(mod.run, 0.99)).
      map(_.show).
      runWith(Streaming.writeStreamToFile("data/PoissonLongForecast.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

/**
  * Perform a one step forecast on the poisson data, using unseen test data,
  * Sampling from the joint posterior of the parameters and the state p(x, theta | y)
  */
object OneStepForecastPoisson extends App with PoissonTestModel with DataProtocols {
  val testData = DataFromFile("data/PoissonModelSims.csv").
    observations.
    drop(400).
    take(100)

  val readPosterior: Future[Seq[MetropState]] = FileIO.fromPath(Paths.get("data/PoissonPosterior.json")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
    map(_.utf8String).
    drop(1000). // discard burn in iterations
    map(_.parseJson.convertTo[MetropState]).
    runWith(Sink.seq)

  // create a distribution over the posterior distribution of the state and parameters
  def createDist[A, B](s: Seq[A])(f: A => B): Rand[B] = new Rand[B] {
    def draw = {
      f(Resampling.sampleOne(s.toVector))
    }
  }

  val t0 = 40.0
  val resample: Resample[(Parameters, State), Id] = Resampling.systematicResampling

  val res = for {
    posterior <- readPosterior
    simPosterior = createDist(posterior)(s => (s.params, s.sde.state))
    filter = ParticleFilter.filterFromPosterior(resample, simPosterior, 1000, t0)(mod.run)
    io <- testData.
      via(filter).
      map(s => PfState(s.t, s.observation, s.ps.map(_._2), s.ll, 0)).
      map(s => ParticleFilter.getMeanForecast(s, mod(poissonParam), s.t + 0.1)).
      map(_.show).
      runWith(Streaming.writeStreamToFile("data/PoissonOneStepForecast.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

/**
  * Run the filter over the last 100 elements of the simulted data using 
  * samples from the joint-posterior of the state and parameters, p(x, theta | y) 
  */
object FilterPoisson extends App with PoissonTestModel with DataProtocols {
  val testData = DataFromFile("data/PoissonModelSims.csv").
    observations.
    drop(400).
    take(100)

  val readPosterior: Future[Seq[MetropState]] = FileIO.fromPath(Paths.get("data/PoissonPosterior.json")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
    map(_.utf8String).
    drop(1000). // discard burn in iterations
    map(_.parseJson.convertTo[MetropState]).
    runWith(Sink.seq)

  // create a distribution over the posterior distribution of the state and parameters
  def createDist[A, B](s: Seq[A])(f: A => B): Rand[B] = new Rand[B] {
    def draw = {
      f(Resampling.sampleOne(s.toVector))
    }
  }

  val t0 = 40.0
  val resample: Resample[(Parameters, State), Id] = Resampling.systematicResampling

  val res = for {
    posterior <- readPosterior
    simPosterior = createDist(posterior)(s => (s.params, s.sde.state))
    filter = ParticleFilter.filterFromPosterior(resample, simPosterior, 1000, t0)(mod.run)
    io <- testData.
      via(filter).
      map(s => PfState(s.t, s.observation, s.ps.map(_._2), s.ll, 0)).
      map(_.show).
      runWith(Streaming.writeStreamToFile("data/PoissonOnlineFilter.csv"))
  } yield io

  res.
    onComplete(_ => system.terminate())
}
