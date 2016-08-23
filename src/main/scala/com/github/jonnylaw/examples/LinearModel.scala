package com.gihub.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import akka.util.ByteString
import GraphDSL.Implicits._
import akka.stream.ClosedShape
import java.nio.file.{Path, Paths}

import com.github.jonnylaw.model._
import com.github.jonnylaw.model.POMP.LinearModel
import com.github.jonnylaw.model.DataTypes._
import com.github.jonnylaw.model.SimData._
import com.github.jonnylaw.model.StateSpace._
import com.github.jonnylaw.model.Streaming._
import java.io.{PrintWriter, File}

trait LinearModel {
  val unparamMod = LinearModel(stepBrownian)
  val p = LeafParameter(GaussianParameter(0.1, 1.0), Some(1.0), BrownianParameter(-0.2, 1.0))
  val mod = unparamMod(p)
  val data: Vector[Data]
  val times = (0.0 to 1.0 by 0.01).toList
  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling, times.min)
  val mll: Int => Parameters => LogLikelihood = n => filter.llFilter(data)(n) _
  val mh: (Double, Int) => MetropolisHastings = (delta, n) => ParticleMetropolis(mll(n), p, Parameters.perturb(delta))

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
  * Test Multiple methods with the same data
  */
object SimLinear extends App {
  val mod = new LinearModel {
    val data = simDataRand(times, mod).draw
  }

  val pw = new PrintWriter("LinearModelSims.csv")
  pw.write(mod.data.mkString("\n"))
  pw.close()
}

/**
  * Breeze MCMC, single site Metropolis Hastings
  * With different values of delta
  */
object BreezeSingleSite {
  def main(args: Array[String]): Unit = {
    val mod = new LinearModel {
      val data = scala.io.Source.fromFile("LinearModelSims.csv").getLines.
        map(a => a.split(",")).
        map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
        toVector
    }

    mod.parser.parse(args, mod.Config()) match {
      case Some(config: mod.Config) =>
        val dn = for {
          d <- config.delta
          n <- config.particles
        } yield (d, n)

        dn map { case (delta, n) =>
          val pw = new PrintWriter("LinearModelBreezeSingleSite.csv")
          pw.write(mod.mh(delta, n).iters.sample(config.iterations).mkString("\n"))
          pw.close()
        }
      case None => // Incorrect arguments
    }
  }
}

/**
  * Breeze MCMC, random walk Metropolis Hastings
  * with different values of delta
  */
object BreezeMCMC {
  def main(args: Array[String]): Unit = {
    val mod = new LinearModel {
      val data = scala.io.Source.fromFile("LinearModelSims.csv").getLines.
        map(a => a.split(",")).
        map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
        toVector
    }

    mod.parser.parse(args, mod.Config()) match {
      case Some(config: mod.Config) =>
        val dn = for {
          d <- config.delta
          n <- config.particles
        } yield (d, n)

        dn map { case (delta, n) =>
          val pw = new PrintWriter("LinearModelBreezeSingleSite.csv")
          pw.write(mod.mh(delta, n).itersVector(config.iterations).mkString("\n"))
          pw.close()
        }
      case None => // Incorrect arguments
    }
  }
}


/**
  * Using akka-streaming, random walk metropolis algorithm
  * with different values of delta 
  */
object StreamingLinearModel extends App {
  implicit val system = ActorSystem("StreamingLinearModel")
  implicit val materializer = ActorMaterializer()

  val mod = new LinearModel {
    val data = scala.io.Source.fromFile("LinearModelSims.csv").getLines.
      map(a => a.split(",")).
      map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
      toVector
  }

  mod.parser.parse(args, mod.Config()) match {
      case Some(config: mod.Config) =>
        val dn = for {
          d <- config.delta
          n <- config.particles
        } yield (d, n)

      Source(dn.toIndexedSeq).
        mapAsync(parallelism = 8){ case (d, n) =>
          mod.mh(d, n).itersAkka.
            take(10000).
            map(p => ByteString(s"$p\n")).
            runWith(FileIO.toPath(Paths.get(s"./LinearModelStreamOut-$d-$n.csv")))
        }.
        runWith(Sink.onComplete { _ =>
          system.shutdown()
        })
    case None => // Incorrect arguments
  }
}

/**
  * Graph DSL version, random walk metropolis algorithm
  */
object StreamingGraph extends App{
  implicit val system = ActorSystem("Graphs")
  implicit val materializer = ActorMaterializer()

  val mod = new LinearModel {
    val data = scala.io.Source.fromFile("LinearModelSims.csv").getLines.
      map(a => a.split(",")).
      map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
      toVector
  }

  def graph(n: Int, particles: Int, delta: Double) = RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>

    val out = FileIO.toPath(Paths.get(s"./LinearModelGraph-$delta-$particles.csv"))
    val bcast = builder.add(Broadcast[MetropState](2))

      mod.mh(delta, particles).itersAkka ~> bcast
    bcast ~> monitorStream(1000, 1) ~> Sink.ignore

    bcast ~> Flow[MetropState].take(n) ~> Flow[MetropState].map(p => ByteString(s"$p\n")) ~> out

    ClosedShape
  })

  mod.parser.parse(args, mod.Config()) match {
    case Some(config: mod.Config) =>
      val dn = for {
        d <- config.delta
        n <- config.particles
      } yield (d, n)

      dn map { case (delta, particles) =>
        graph(config.iterations, particles, delta).run()
      }
    case None => // Incorrect arguments
  }
}
