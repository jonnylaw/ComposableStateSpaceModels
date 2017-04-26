package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import DataProtocols._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.json._

import java.nio.file.Paths

/**
  * Perform a pilot run, by running the particle filter over the data multiple times
  * to determine the variance of the estimate of the log-likelihood using different amount of particles.
  * In order to run the PMMH algorithm quickly, a low number of particles is required, but an accurate 
  * estimate of the likelihood is also required. 
  * A rule of thumb for the variance of the estimated log-likelihood is 1.0
  */
object PilotRun extends App with TestNegBinMod {
  implicit val system = ActorSystem("PilotRun")
  implicit val materializer = ActorMaterializer()

  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[State] = Resampling.systematicResampling _

  val res = for {
    data <- DataFromFile("data/NegBin/NegativeBinomial.csv").
      observations.
      zip(Source(Stream.from(1))).
      filter { case (_, i) => i % 10 == 0 }.
      map { case (data, _) => data }.
      runWith(Sink.seq)
    vars = Streaming.pilotRun(data.toVector, model, params, resample, particles, 100)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/NegBin/NegativeBinomialPilotRun.csv"))
  } yield io

  res.onComplete { s =>
    println(s)
    system.terminate()
  }
}

/**
  * Run the PMMH algorithm over the first 400 simulated observations
  * 1. Read the data in from a file
  * 2. group the first 400 observations into a Seq[Data]
  * 3. use mapConcat to get a tuple containing the MCMC chain identifier and a Seq[Data]
  * 4. Use mapAsync to run two chains in parallel
  * 5. Write the chains to file in JSON format
  */
object DeterminePosterior extends App with TestNegBinMod {
  implicit val system = ActorSystem("PMMH")
  implicit val materializer = ActorMaterializer()

  val resample: Resample[State] = Resampling.systematicResampling _
  val prior = (p: Parameters) => 0.0

  DataFromFile("data/NegBin/NegativeBinomial.csv").
    observations.
    zip(Source(Stream.from(1))).
    filter { case (_, i) => i % 10 == 0 }.
    map { case (data, _) => data }.
    take(400).
    grouped(400).
    mapConcat(data => (1 to 2).map(chain => (chain, data))).
    mapAsync(2) { case (chain, d) =>
      val filter = ParticleFilter.filterLlState(d.toVector, resample, 100)
      val pf = filter compose model
      val pmmh = MetropolisHastings.pmmhState(params, Parameters.perturb(0.05), (a, b) => 0.0, prior)

      pmmh(pf).
        via(Streaming.monitorStateStream).async.
        take(100000).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile(s"data/NegBin/NegativeBinomialPosterior-$chain.json"))
    }.
    runWith(Sink.onComplete { s => 
      println(s)
      system.terminate()
    })
}

/**
  * Convert the JSON files to CSVs
  */
object JsonToCSV {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("PMMH")
    implicit val materializer = ActorMaterializer()

    val files = List("data/NegBin/NegativeBinomialPosterior-1.json", "data/NegBin/NegativeBinomialPosterior-2.json")

    Future.sequence(files.zipWithIndex map { case (file, i) =>
      Streaming.jsonToCSV(file, s"data/NegBin/NegativeBinomialPosterior-$i.csv")
    }).onComplete { s =>
      println(s)
      system.terminate()
    }
  }
}
