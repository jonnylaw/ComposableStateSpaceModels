package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import scala.concurrent.Future
import akka.actor.ActorSystem
import cats.implicits._
import com.github.jonnylaw.model._
import DataProtocols._
import CsvFormatShow._

/**
  * Specify a model to use
  */
trait TestModel {
  val sde = Sde.ouProcess(1)
  val sdeParameters = SdeParameter.ouParameter(1.0)(0.5)(0.2)(1.5)(0.05)

  val sde2 = Sde.ouProcess(8)
  val sde2Parameters = SdeParameter.ouParameter(1.0)(2.0)(0.2)(-4, -4, 0, 0, 0, 0, -0.5, -0.5)(0.3)

  val modelName = "NegativeBinomial"

  val negbinMod = Model.negativeBinomial(sde)
  val negbinParam = Parameters(Some(2.0), sdeParameters)
  val seasonalMod = Model.seasonal(24, 4, sde2)
  val seasonalParam = Parameters(None, sde2Parameters)
  val params = negbinParam |+| seasonalParam
  val model: UnparamModel = negbinMod |+| seasonalMod
}

object SimOrnstein extends App with TestModel {
  implicit val system = ActorSystem("SimulateOU")
  implicit val executionContext = system.dispatcher

  Future.fromTry(sde2(sde2Parameters)).flatMap(s =>
    s.simStream(0.0, 0.01).
    take(5000).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    runWith(Streaming.writeStreamToFile("data/ornsteinUhlenbeck.csv"))).
    onComplete(_ => system.terminate())
}

/**
  * Simulate Data from the Test model and write it to CSV and JSON files asynchronously
  */
object SimulateModel extends App with TestModel {
  implicit val system = ActorSystem("SimulateModel")
  implicit val executionContext = system.dispatcher

  Future.fromTry(model(params)).flatMap(m => SimulateData(m).
    observations.
    take(5000).
    alsoTo(Streaming.dataCsvSink(s"data/${modelName}_sims.csv")).
    toMat(Streaming.dataJsonSink(s"data/${modelName}_sims.json"))(Keep.right).
    run()).
    onComplete(_ => system.terminate())
}

object SimulateSeasonal extends App {
  implicit val system = ActorSystem("SimulateSeasonal")
  implicit val executionContext = system.dispatcher

  val sde = Sde.ouProcess(6)
  val model = Model.seasonal(24, 3, sde)
  val sdeParameters = SdeParameter.
    ouParameter(0.1)(1.0)(0.4)(0.1)(0.5)

  val params = Parameters(Some(1.0), sdeParameters)

  Future.fromTry(model(params)).flatMap(m => SimulateData(m).
    observations.
    take(1000).
    toMat(Streaming.dataCsvSink(s"data/seasonal_sims.csv"))(Keep.right).
    run()).
    onComplete(_ => system.terminate())
}

object SimulateLgcp extends App {
  implicit val system = ActorSystem("SimulateLGCP")
  implicit val executionContext = system.dispatcher

  val sde = Sde.ouProcess(1)
  val model = Model.lgcp(sde)
  val sdeParameters = SdeParameter.
    ouParameter(0.1)(0.5)(0.4)(0.1)(0.5)
  val params = Parameters(None, sdeParameters)

  val events = SimulateData(model(params).get).
    simLGCP(0.0, 10.0, 2)

  Source(events).
    toMat(Streaming.dataCsvSink(s"data/lgcp_sims.csv"))(Keep.right).
    run().
    onComplete(_ => system.terminate())
}
