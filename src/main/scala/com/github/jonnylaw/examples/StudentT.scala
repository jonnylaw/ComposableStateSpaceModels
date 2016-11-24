package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import java.nio.file.Paths
import akka.stream.scaladsl._
import akka.util.ByteString
import scala.concurrent.ExecutionContext.Implicits.global

import com.github.jonnylaw.model._
import UnparamModel._
import Streaming._
import DataTypes._
import SimData._
import Utilities._
import State._
import Parameters._
import StateSpace._
import java.io.{PrintWriter, File}
import breeze.linalg.{DenseVector, diag}
import cats.implicits._

trait TModel {
  val tparams: Parameters = LeafParameter(
    GaussianParameter(0.0, 3.0),
    Some(0.3),
    OrnsteinParameter(3.0, 1.0, 0.5))
  val seasParams: Parameters = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.0), diag(DenseVector.fill(6)(3.0))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(2.0), DenseVector.fill(6)(0.5), DenseVector.fill(6)(0.3)))

  val p = tparams |+| seasParams

  val st: UnparamModel = studentTModel(stepOrnstein, 5)
  val seasonal: UnparamModel = SeasonalModel(24, 3, stepOrnstein)

  val unparamMod = st |+| seasonal
  val mod = unparamMod(p)
}

object SeasStudentT extends App with TModel {
  implicit val system = ActorSystem("SimulateSeasT")
  implicit val materializer = ActorMaterializer()

  val times = (1 to 7*24).map(_.toDouble).toList

  // simulate from the Student T POMP model, simulating states and observations at the times above
  Source(times).
    via(mod.simPompModel(0.0)).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("seastdistSims.csv"))).
    onComplete(_ => system.terminate)
}

object GetSeasTParams extends App with TModel {
  implicit val system = ActorSystem("SeasTPmmh")
  implicit val materializer = ActorMaterializer()

  // read the data in as a stream
  val data = FileIO.fromPath(Paths.get("seastdistsims.csv")).
    via(Framing.delimiter(
      ByteString("\n"),
      maximumFrameLength = 256,
      allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)

  data.
    take(168).
    grouped(168).
    map(d => {
      val mll = filter.llFilter(d.toVector, d.map(_.t).min)(200) _

      ParticleMetropolis(mll, p, Parameters.perturb(0.05)).
        iters.
        take(10000).
        map(s => ByteString(s + "\n")).
        runWith(FileIO.toPath(Paths.get("seastMCMC.csv"))).
        onComplete(_ => system.terminate)
    }).
    runWith(Sink.ignore)
}
