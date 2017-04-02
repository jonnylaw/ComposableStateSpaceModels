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
  val poissonParam: Parameters = Parameters.leafParameter(None, 
    SdeParameter.ouParameter(m0 = 0.5, c0 = 0.12, theta = 2.3, alpha = 0.2, sigma = 0.5))

  val mod = Model.poissonModel(Sde.ouProcess(1))

  val prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(None, OuParameter(m, s, theta, alpha, sigma)) =>
      Gaussian(0.5, 3.0).logPdf(m) +
      Gamma(1.0, 1.0).logPdf(s) + 
      Gaussian(2.3, 1.0).logPdf(theta) +
      Gamma(0.1, 2.0).logPdf(alpha) +
      Gamma(0.25, 2.0).logPdf(sigma)
  }

  val simPrior = {
    for {
      m <- Gaussian(0.5, 3.0)
      c <- Gamma(1.0, 1.0)
      theta <- Gaussian(2.5, 1.0)
      alpha <- Gamma(0.2, 1.0)
      sigma <- Gamma(0.5, 1.0)
    } yield Parameters.leafParameter(None, 
      SdeParameter.ouParameter(m, c, theta, alpha, sigma))
  }

  implicit val system = ActorSystem("PoissonModel")
  implicit val materializer = ActorMaterializer()
}

object SimPoissonModel extends App with PoissonTestModel {
  SimulateData(mod(poissonParam)).
    observations.
    take(500).
    map((d: Data) => d.show).
    runWith(Streaming.writeStreamToFile("data/PoissonModelSims.csv")).
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
  val resample: Resample[State, Id] = Resampling.systematicResampling _

  val res = for {
    data <- dataStream
    out = Streaming.pilotRun(data.toVector, mod, poissonParam, resample, particles, 100)
    io <- out.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/PoissonPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

