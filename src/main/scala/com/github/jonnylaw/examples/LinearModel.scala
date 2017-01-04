package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString

import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file.Paths
import scala.util.{Try, Success}
import scala.concurrent.Future
import akka.stream.IOResult

import com.github.jonnylaw.model._
import DataTypes._
import StateSpace._
import Model._
import cats.implicits._ // this imports extra good syntax for Either

trait LinearModel {
  val unparamMod = LinearModel(stepBrownian)
  val p = LeafParameter(GaussianParameter(0.1, 1.0), Some(1.0), BrownianParameter(-0.2, 1.0))
  val mod = unparamMod(p)
}

/**
  * Simulate from the linear model
  */
object SimLinear extends App with LinearModel {
  implicit val system = ActorSystem("SimulateLinearModel")
  implicit val materializer = ActorMaterializer()

  val res: Either[Throwable, Future[IOResult]] = mod.
    right.
    map(_.simRegular(1.0).
    take(1000).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("LinearModelSims.csv")))
  )

  res.
    right.
    map(_.onComplete(_ => system.terminate))
}

/**
  * Random walk Metropolis Hastings
  * with different values of delta
  */
object BreezeMCMC extends App with LinearModel {
  implicit val system = ActorSystem("PMMH")
  implicit val materializer = ActorMaterializer()

  val dn = for {
    d <- List(0.05, 0.1, 0.25)
    n <- List(200, 500, 1000)
  } yield (d, n)

  // Read in the data
  val data = FileIO.fromPath(Paths.get("LinearModelSims.csv")).
    via(Framing.delimiter(
      ByteString("\n"),
      maximumFrameLength = 256,
      allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))


  Source(dn).mapAsync(parallelism = 8){ case (delta, n) =>
    
    data.
      take(100).
      grouped(100).
      map((d: Seq[Data]) => {

        def mll(n: Int)(p: Parameters) = {
          for {
            mod <- unparamMod(p)
            filter = Filter(mod, ParticleFilter.multinomialResampling)
          } yield filter.llFilter(d, 0.0)(n)
        }

        MetropolisParams(mll(n), Parameters.perturb(delta), p).
          iters.
          take(10000).
          map(s => ByteString(s + "\n")).
          runWith(FileIO.toPath(Paths.get(s"LinearModelPMMH_${delta}_${n}.csv"))).
          onComplete(_ => system.terminate)
      }).
      runWith(Sink.ignore)
  }
}
