package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.NotUsed
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.instances._
import cats.implicits._
import com.github.jonnylaw.model._
import DataProtocols._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.json._

/**
  * Filter using the same parameters that were used to simulate the model:
  * 1. Read in data simulated from the model, using DataFromFile
  * 2. Define the particle filter and the start time for the filter
  * 3. Run the filter using the same parameters we simulated the model with
  * 4. Save the output to a file asynchronously
  */
object Filtering extends App with TestModel {
  implicit val system = ActorSystem("Filtering")
  implicit val materializer = ActorMaterializer()

  val data = DataFromJson(s"data/${modelName}_sims.json").
    observations

  val t0 = 0.0
  val filter = ParticleFilter.filter(Resampling.systematicResampling, t0, 1000) compose model

  data.
    via(filter(params)).
    map(ParticleFilter.getIntervals(model, params)).
    map(_.show).
    runWith(Streaming.writeStreamToFile(s"data/${modelName}Filtered.csv")).
    onComplete(_ => system.terminate())
}

/**
  * Once the posterior distribution of the state and parameters, p(x, theta | y) has been
  * determined, filtering can be performed online 
  * 1. Read in the test data, dropping the first 4000 elements which are used to determine the posterior
  * 2. Set up the filter
  * 3. Read in the posterior distribution from a JSON file
  * 4. Run several filters each starting with a sample (x, theta) from the joint posterior of the state and parameters
  * 5. Write the results of the filter to a file
  */
object OnlineFiltering extends App with TestModel {
  implicit val system = ActorSystem("OnlineFiltering")
  implicit val materializer = ActorMaterializer()

  val testData = DataFromJson(s"data/${modelName}_sims.json").
    observations.
    drop(4000)

  val resample: Resample[State] = Resampling.systematicResampling _

  val res = for {
    // get the starting time of the data Stream
    t0 <- testData.map(_.t).runWith(Streaming.minSink)

    // read in the posterior distribution from a file
    posterior <- Streaming.readPosterior(s"data/${modelName}Posterior-1.json", 10000, 2).
      runWith(Sink.seq)
    simPosterior = Streaming.createDist(posterior)(x => (x.sde.state, x.params))

    // run the particle filter and write the results to a file
    io <- testData.
      via(ParticleFilter.filterOnline(resample, t0, 1000, simPosterior.sample(100).toVector, model)).
      map(_.show).
      runWith(Streaming.writeStreamToFile(s"data/${modelName}OnlineFilter.csv"))
  } yield io

  res.
    onComplete(s => {
      println(s)
      system.terminate()
    })
}
