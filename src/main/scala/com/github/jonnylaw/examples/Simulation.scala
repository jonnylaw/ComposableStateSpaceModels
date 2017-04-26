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
import scala.concurrent.Future

/**
  * Specify a model to use 
  */
trait TestNegBinMod {
  val sde = Sde.brownianMotion(1)
  val sdeParam = SdeParameter.brownianParameter(0.0)(log(1.0))(log(0.01))
  val p = Parameters.leafParameter(Some(log(3.0)), sdeParam)

  val sde2 = Sde.ouProcess(8)
  val sde2Param = SdeParameter.ouParameter(0.0)(log(1.0))(log(0.3))(log(0.1))(1.5, 1.5, 1.0, 1.0, 1.5, 1.5, 0.1, 0.1)
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
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    runWith(Streaming.writeStreamToFile("data/ornsteinUhlenbeck.csv")).
    onComplete(_ => system.terminate())
}

/**
  * Simulate Data from the Negative Binomial model and write it to CSV and JSON files asynchronously
  *
  */
object SimulateNegativeBinomial extends App with TestNegBinMod {
  implicit val system = ActorSystem("SimulateToJson")
  implicit val materializer = ActorMaterializer()

  SimulateData(model(params)).
    observations.
    take(5000).
    alsoTo(Streaming.dataCsvSink("data/NegBin/NegativeBinomial.csv")).
    toMat(Streaming.dataJsonSink("data/NegBin/NegativeBinomial.csvb"))(Keep.right).
    run().
    onComplete(_ => system.terminate())
}
