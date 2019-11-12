package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import cats.implicits._
import com.github.jonnylaw.model._
import CsvFormatShow._
import scala.concurrent.Future

/**
  * Filter using the same parameters that were used to simulate the model:
  * 1. Read in data simulated from the model, using DataFromFile
  * 2. Define the particle filter and the start time for the filter
  * 3. Run the filter using the same parameters we simulated the model with
  * 4. Save the output to a file asynchronously
  */
object Filtering extends App with TestModel {
  implicit val system = ActorSystem("Filtering")
  implicit val executionContext = system.dispatcher

  val data = DataFromJson(s"data/${modelName}_sims.json").
    observations

  val t0 = 0.0
  val filter = ParticleFilter.filter(Resampling.systematicResampling, t0, 1000)

  Future.fromTry(model.run(params)).flatMap { mod =>
    data.
      via(filter(mod)).
      map(s => ParticleFilter.getIntervals(mod, s)).
      map(_.show).
      runWith(Streaming.writeStreamToFile(s"data/${modelName}Filtered.csv"))
  }.onComplete(_ => system.terminate())
}
