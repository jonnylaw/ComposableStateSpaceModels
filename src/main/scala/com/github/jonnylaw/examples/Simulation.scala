package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import breeze.numerics.log
import breeze.linalg._
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
  val sdeParam = SdeParameter.brownianParameter(0.0)(log(1.0))(log(0.01))
  val p = Parameters.leafParameter(Some(log(3.0)), sdeParam)

  val sde2 = Sde.ouProcess(8)
  val sde2Param = SdeParameter.ouParameter(0.0)(log(1.0))(log(0.3))(log(0.1))(1.0, 2.0)
  val p1 = Parameters.leafParameter(None, sde2Param)    

  val params = p |+| p1
  
  val model = Model.negativeBinomial(sde) |+| Model.seasonalModel(24, 4, sde2)
}

object SimOrnstein extends App with TestNegBinMod {
  implicit val system = ActorSystem("SimulateOU")
  implicit val materializer = ActorMaterializer()

  sde2(sde2Param).
    simStream(0.0, 0.01).
    take(5000).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/ornsteinUhlenbeck.csv")).
    onComplete(_ => system.terminate())
}

object SimModelToCSV extends App with TestNegBinMod {
  implicit val system = ActorSystem("SimulateToCSV")
  implicit val materializer = ActorMaterializer()

  SimulateData(model(params)).
    observations.
    take(5000).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/NegBin/NegativeBinomial.csv")).
    onComplete(_ => system.terminate())
}

object SimModelToJSON extends App with TestNegBinMod {
  implicit val system = ActorSystem("SimulateToJson")
  implicit val materializer = ActorMaterializer()

  SimulateData(model(params)).
    observations.
    take(5000).
    map(_.toJson.compactPrint).
    runWith(Streaming.writeStreamToFile("data/NegBin/NegativeBinomial.json")).
    onComplete(_ => system.terminate())
}
