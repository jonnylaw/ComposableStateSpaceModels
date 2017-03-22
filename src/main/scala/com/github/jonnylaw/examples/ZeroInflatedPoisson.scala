package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.util.ByteString
import akka.actor.ActorSystem
import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.linalg.cholesky
import breeze.stats.distributions._
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import spray.json._

trait ZipModel {
  val p = Parameters.leafParameter(
    Some(log(0.2)),
    SdeParameter.ouParameter(m0 = 0.0, c0 = log(1.0), theta = 2.0, alpha = log(0.25), log(0.5)))

  val model = Model.zeroInflatedPoisson(Sde.ouProcess(1))

  // actor system
  implicit val system = ActorSystem("ZipModel")
  implicit val materializer = ActorMaterializer()

  val prior = (p: Parameters) => 0.0
}

object SimZipModel extends App with ZipModel with DataProtocols {
  SimulateData(model(p)).
    observations.
    take(500).
    map(_.toJson.compactPrint).
    runWith(Streaming.writeStreamToFile("data/ZiPoissonModel.json")).
    onComplete(_ => system.terminate())
}

object PilotRunZip extends App with ZipModel with DataProtocols {
  DataFromJson("data/ZiPoissonModel.json").observations.take(400).runWith(Sink.seq).flatMap ( data =>
    Streaming.pilotRun(data.toVector, model, p, Resampling.stratifiedResampling, Vector(100, 200, 500, 1000, 2000)).
      map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/PilotRunZip.csv"))
  ).
    onComplete(_ => system.terminate())

}

object ZipModelPosterior extends App with ZipModel with DataProtocols {
  def iters(chain: Int): Future[IOResult] = for {
    data <- DataFromJson("data/ZiPoissonModel.json").observations.take(400).runWith(Sink.seq)
    pf = (param: Parameters) => ParticleFilter.filterLlState(data.toVector, Resampling.systematicResampling, 100)(model(param))
    pmmh = ParticleMetropolisState(pf, p, Parameters.perturbMvn(cholesky(DenseMatrix.eye[Double](p.length))), prior)
    io <- pmmh.
      iters.
      take(10000).
      map(_.toJson).
      map(_.toString).
      runWith(Streaming.writeStreamToFile(s"data/ZiPoissonPosterior-$chain.json"))
  } yield io

  Future.sequence((1 to 4) map (iters)).
    onComplete(_ => system.terminate())
}

object WritePosterior extends App with DataProtocols with ZipModel {
  FileIO.fromPath(Paths.get("data/ZiPoissonPosterior-1.json")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
    map(_.utf8String).
    map(_.parseJson.convertTo[MetropState]).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/ZiPoissonPosterior.csv")).
    onComplete(_ => system.terminate())  
}
