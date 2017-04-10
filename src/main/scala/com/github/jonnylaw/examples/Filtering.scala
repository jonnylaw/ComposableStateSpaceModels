package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.json._

/**
  * Filter using the same parameters that were used to simulate the model:
  * 1. Read in data simulated from the model, using DataFromFile
  * 2. Define the particle filter and the start time for the filter
  * 3. 
  */
object Filtering extends App with TestNegBinMod with DataProtocols {
  implicit val system = ActorSystem("Filtering")
  implicit val materializer = ActorMaterializer()

  val data = DataFromFile("data/NegativeBinomial.csv").observations

  val t0 = 0.0
  val filter = ParticleFilter.filter(Resampling.systematicResampling, t0, 1000)

  data.
    via(filter(model(params))).
    map(ParticleFilter.getIntervals(model(params))).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/NegativebinomialFiltered.csv")).
    onComplete(_ => system.terminate())
}

/**
  * Once the posterior distribution of the state and parameters, p(x, theta | y) has been
  * determined, filtering can be performed online 
  * 1. Read in the test data, dropping the first 400 elements which are used to determine the posterior
  * 2. Set up the filter
  * 3. Read in the posterior distribution from a JSON file
  * 4.
  */
object OnlineFiltering extends App with TestNegBinMod with DataProtocols {
  implicit val system = ActorSystem("OnlineFiltering")
  implicit val materializer = ActorMaterializer()

  val testData = DataFromFile("data/NegativeBinomial.csv").
    observations.
    drop(4000)

  val t0 = 400 * 0.1

  val resample: Resample[State] = Resampling.systematicResampling _

  val res = for {
    posterior <- Streaming.readPosterior("data/NegativeBinomialPosterior-1.json", 10000, 2).
      runWith(Sink.seq)
    simPosterior = Streaming.createDist(posterior)(x => (x.params, x.sde.state))
    io <- testData.
    take(2).
      via(ParticleFilter.filterOnline(simPosterior, 2, t0, 100, resample, model.run)).
      map(_.show).
      runForeach(println)
  } yield io


    // runWith(Streaming.writeStreamToFile("data/NegativeBinomialOnlineFilter.csv")).

  res.
    onComplete(s => {
      println(s)
      system.terminate()
    })
}
