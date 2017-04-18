package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import DataProtocols._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._

/**
  * Specify a model to use 
  */
trait TestNegBinMod {
  val sde = Sde.brownianMotion(1)
  val p = Parameters.leafParameter(Some(log(3.0)), 
    SdeParameter.brownianParameter(0.0, log(1.0), log(0.01)))

  val sde2 = Sde.ouProcess(8)
  val p1 = Parameters.leafParameter(None,
    SdeParameter.ouParameter(0.0, log(1.0), log(0.3), log(0.01))
      (1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 0.1, 0.1))

  val params = p |+| p1
  
  val model = Model.negativeBinomial(sde) |+| Model.seasonalModel(24, 4, sde2)
}

object SimModelToCSV extends App with TestNegBinMod {
  implicit val system = ActorSystem("SimulateToCSV")
  implicit val materializer = ActorMaterializer()

  SimulateData(model(params).right.get).
    observations.
    take(5000).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/NegativeBinomial.csv")).
    onComplete(_ => system.terminate())
}

object SimModelToJSON extends App with TestNegBinMod {
  implicit val system = ActorSystem("SimulateToJson")
  implicit val materializer = ActorMaterializer()

  SimulateData(model(params).right.get).
    observations.
    take(5000).
    map(_.toJson.compactPrint).
    runWith(Streaming.writeStreamToFile("data/NegativeBinomial.json")).
    onComplete(_ => system.terminate())
}
