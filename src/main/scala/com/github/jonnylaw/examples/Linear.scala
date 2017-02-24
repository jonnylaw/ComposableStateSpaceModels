package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.ExecutionContext.Implicits.global

trait LinearTestModel {
  val linearParam: Parameters = Parameters.leafParameter(
    Some(1.0),
    SdeParameter.brownianParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      mu = DenseVector(0.01),
      sigma = DenseVector(0.5))
  )

  val mod = Model.linearModel(Sde.brownianMotion)

  implicit val system = ActorSystem("LinearModel")
  implicit val materializer = ActorMaterializer()
}

object SimLinearModel extends App with LinearTestModel {
  SimulateData(mod(linearParam)).
    observations.
    take(200).
    map((d: Data) => d.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/LinearModelSims.csv"))).
    onComplete(_ => system.terminate())
}

object FilterLinear extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations
    
  val t0 = 0.0

  val filter = ParticleFilter.filter[ParVector](
    ParticleFilter.parMultinomialResampling, t0, 1000)

  data.
    via(filter(mod(linearParam))).
    map(ParticleFilter.getIntervals(mod(linearParam))).
    map(_.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/LinearModelFiltered.csv"))).
    onComplete(_ => system.terminate())
}

/**
  * Perform a pilot run of the PMMH algorithm, to determine the optimum number of particles to use in the particle filter
  */
object PilotRunLinear extends App with LinearTestModel {
  val particles = Vector(100, 200, 500, 1000, 2000)

  val res = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun(data.toVector, mod, linearParam, particles)
    io <- vars.map { case (n, v) => s"$n, $v" }.runWith(Streaming.writeStreamToFile("data/LinearPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

object DetermineLinearParameters extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations

  def prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(v), BrownianParameter(m, c, mu, sigma)) =>
      Gamma(0.2, 5.0).logPdf(v) + 
      Gaussian(0.5, 2.0).logPdf(m(0)) +
      Gamma(0.2, 5.0).logPdf(c(0)) + 
      Gaussian(0.01, 2.0).logPdf(mu(0)) +
      Gamma(0.2, 2.5).logPdf(sigma(0))
  }
  
  data.
    take(200).
    grouped(200).
    map { (d: Seq[Data]) =>

      val filter = ParticleFilter.likelihood[ParVector](d.toVector,
        ParticleFilter.systematicResampling, 150).compose(mod)

      ParticleMetropolis(filter.run, linearParam, Parameters.perturb(0.05), prior).
        params.
        take(10000).
        map(_.show).
        runWith(Streaming.writeStreamToFile("data/LinearModelParams.csv")).
        onComplete(_ => system.terminate())
    }.
    runWith(Sink.ignore)
}

/**
  * Perform a one step forecast of the data
  */
object OneStepForecastLinear extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.parMultinomialResampling, 0.0, 1000).
    compose(mod)

  // set the prediction interval, predict five minutes ahead from each observation
  val dt = 5.0/60

  data.
    via(filter(linearParam)).
    map(s => ParticleFilter.getMeanForecast(s, mod(linearParam), s.t + dt)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearModelForecast.csv")).
    onComplete(_ => system.terminate())
}
