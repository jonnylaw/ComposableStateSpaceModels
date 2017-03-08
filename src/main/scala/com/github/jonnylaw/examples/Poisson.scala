package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats._
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import java.io._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait PoissonTestModel {
  val poissonParam: Parameters = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      theta = DenseVector(2.3),
      alpha = DenseVector(0.2),
      sigma = DenseVector(0.5))
  )

  val mod = Model.poissonModel(Sde.ornsteinUhlenbeck)

  val prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(None, OrnsteinParameter(m, s, theta, alpha, sigma)) =>
      Gaussian(0.5, 3.0).logPdf(m(0)) +
      Gamma(1.0, 1.0).logPdf(s(0)) + 
      Gaussian(2.3, 1.0).logPdf(theta(0)) +
      Gamma(0.1, 2.0).logPdf(alpha(0)) +
      Gamma(0.25, 2.0).logPdf(sigma(0))
  }

  val simPrior = {
    for {
      m <- Gaussian(0.5, 3.0)
      c <- Gamma(1.0, 1.0)
      theta <- Gaussian(2.5, 1.0)
      alpha <- Gamma(0.2, 1.0)
      sigma <- Gamma(0.5, 1.0)
    } yield Parameters.leafParameter(None, 
      SdeParameter.ornsteinParameter(
        DenseVector(m),
        DenseVector(c),
        DenseVector(theta),
        DenseVector(alpha),
        DenseVector(sigma)))
  }

  implicit val system = ActorSystem("PoissonModel")
  implicit val materializer = ActorMaterializer()
}

object SimPoissonModel extends App with PoissonTestModel {
  SimulateData(mod(poissonParam)).
    observations.
    take(500).
    map((d: Data) => d.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/PoissonModelSims.csv"))).
    onComplete(_ => system.terminate())
}

/**
  * Determine how many particles are required to run the MCMC
  */
object PilotRunPoisson extends App with PoissonTestModel {
  val dataStream = DataFromFile("data/PoissonModelSims.csv").
    observations.
    runWith(Sink.seq)

  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[State, Id] = Resampling.treeSystematicResampling _

  val res = for {
    data <- dataStream
    out = Streaming.pilotRun(data.toVector, mod, poissonParam, resample, particles)
    io <- out.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/PoissonPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

