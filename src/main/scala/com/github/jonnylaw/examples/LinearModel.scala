package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import akka.util.ByteString
import GraphDSL.Implicits._
import akka.stream.ClosedShape
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file.{Path, Paths}

import com.github.jonnylaw.model._
import DataTypes._
import SimData._
import StateSpace._
import Streaming._

trait LinearModel {
  val unparamMod = LinearModel(stepBrownian)
  val p = LeafParameter(GaussianParameter(0.1, 1.0), Some(1.0), BrownianParameter(-0.2, 1.0))
  val mod = unparamMod(p)
  val times = (0.0 to 1.0 by 0.01).toList
  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)

  case class Config(delta: Seq[Double] = Seq(0.1, 0.2), particles: Seq[Int] = Seq(200, 500), iterations: Int = 10000)

  val parser = new scopt.OptionParser[Config]("LinearModel") {
    head("LinearModel", "1.0")

    opt[Seq[Double]]('d', "delta") required () action { (x, c) =>
      c.copy(delta = x)
    } text ("delta is a list of perturbation size")

    opt[Seq[Int]]('p', "particles") required () action { (x, c) =>
      c.copy(particles = x)
    } text ("particles is a list of particle cloud size")

    opt[Int]('n', "iterations") action { (x, c) =>
      c.copy(iterations = x)
    } text ("number of MCMC iterations to write to file")
  }
}

/**
  * Simulate from the linear model
  */
object SimLinear extends App with LinearModel {
  implicit val system = ActorSystem("Simulate Linear Model")
  implicit val materializer = ActorMaterializer()

  Source(times).
    via(mod.simPompModel(0.0)).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("LinearModelSims.csv"))).
    onComplete(_ => system.terminate)
}

/**
  * Random walk Metropolis Hastings
  * with different values of delta
  */
object BreezeMCMC extends App with LinearModel {
  implicit val system = ActorSystem("PMMH")
  implicit val materializer = ActorMaterializer()

  parser.parse(args, Config()) match {
    case Some(config: Config) =>
      val dn = for {
        d <- config.delta
        n <- config.particles
      } yield (d, n)

      dn map { case (delta, n) =>
        val data = FileIO.fromPath(Paths.get("LinearModelSims.csv")).
          via(Framing.delimiter(
            ByteString("\n"),
            maximumFrameLength = 256,
            allowTruncation = true)).
          map(_.utf8String).
          map(a => a.split(",")).
          map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

        val mll: (Vector[Data], Int) => Parameters => LogLikelihood = (data, n) => filter.llFilter(data, times.min)(n) _
        
        data.
          take(100).
          grouped(100).
          map((d: Seq[Data]) =>

            ParticleMetropolis(mll(d.toVector, n), p, Parameters.perturb(delta)).
              iters.
              take(config.iterations).
              map(s => ByteString(s + "\n")).
              runWith(FileIO.toPath(Paths.get("LinearModelPMMH.csv"))).
              onComplete(_ => system.terminate)
          ).
          runWith(Sink.ignore)

      }
    case None => // Incorrect arguments
  }
}
