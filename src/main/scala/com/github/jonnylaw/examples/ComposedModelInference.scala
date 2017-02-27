package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString


import breeze.linalg.DenseVector
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import cats.data.Reader
import cats.implicits._
import cats._

import com.github.jonnylaw.model._
import Parameters._

import java.nio.file.Paths
import java.io._

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Define a model to use throughout the examples in this file
  */
trait TestModel {
  val poissonParams = Parameters.leafParameter(
    None,
    SdeParameter.brownianParameter(
      DenseVector(0.0), 
      DenseVector(1.0), 
      DenseVector(0.01), 
      DenseVector(0.01)))

  val seasonalParams = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      DenseVector.fill(6)(1.0),
      DenseVector.fill(6)(1.0),
      theta = DenseVector.fill(6)(1.0),
      alpha = DenseVector.fill(6)(0.5),
      sigma = DenseVector.fill(6)(0.3))
  )

  val params = poissonParams |+| seasonalParams
  val poisson = Model.poissonModel(Sde.brownianMotion)
  val seasonal = Model.seasonalModel(24, 3, Sde.ornsteinUhlenbeck)
  val model = poisson |+| seasonal

  implicit val system = ActorSystem("ComposedModel")
  implicit val materializer = ActorMaterializer()
}

/**
  * Simulate a poisson model, with seasonal rate parameter
  */
object SimulateSeasonalPoisson extends App with TestModel {
  val times = (0.0 to 240.0 by 1.0).
    filter(a => scala.util.Random.nextDouble < 0.9). // 10% of data missing
    toList

  Source.apply(times).
    via(SimulateData(model(params)).simPompModel(0.0)).
    map((x: Data) => x.show).
    runWith(Streaming.writeStreamToFile("data/seasonalPoissonSims.csv")).
    onComplete(_ => system.terminate())
}

object FilterSeasonalPoisson extends TestModel {
  def main(args: Array[String]): Unit = {
    val n = 1000 // number of particles for the particle filter
    val t0 = 0.0 // starting time of the filter (time of the first observation)
    setParallelismGlobally(8) // set the number of threads to use for the parallel particle filter
    val filter = ParticleFilter.filter[ParVector](Resampling.parMultinomialResampling, 0.0, n)

    DataFromFile("data/seasonalPoissonSims.csv").
      observations.
      via(filter(model(params))).
      map(ParticleFilter.getIntervals(model(params))).
      drop(1).
      map(_.show).
      runWith(Streaming.writeStreamToFile("data/seasonalPoissonFiltered.csv")).
      onComplete(_ => system.terminate())
  }
}

object DetermineComposedParams extends TestModel {
  def main(args: Array[String]) = {
    // read a file from memory

    // choose a prior

    def priorDrift(p: Parameters) = p match {
      case LeafParameter(_, BrownianParameter(m0, c0, mu, sigma)) =>
        Some(
          Gaussian(1.0, 3.0).logPdf(m0(0)) +
            Gamma(0.5, 2.0).logPdf(c0(0)) +
            Gaussian(0.0, 3.0).logPdf(mu(0)) +
            Gamma(0.1, 3.0).logPdf(sigma(0)))
      case _ => None
    }

    def priorDaily(p: Parameters) = p match {
      case LeafParameter(_, OrnsteinParameter(m0, c0, theta, alpha, sigma)) =>
        Some(
          m0.mapValues(m => Gaussian(-0.5, 3.0).logPdf(m)).reduce(_+_) +
            c0.mapValues(c => Gamma(0.3, 3.0).logPdf(c)).reduce(_+_) +
            theta.mapValues(m => Gaussian(-1.0, 3.0).logPdf(m)).reduce(_+_) +
            alpha.mapValues(a => Gamma(0.15, 3.0).logPdf(a)).reduce(_+_) +
            sigma.mapValues(s => Gamma(1.0, 3.0).logPdf(s)).reduce(_+_))
      case _ => None
    }

    def prior = (p: Parameters) => p match {
      case BranchParameter(drift, daily) =>
        for {
          p1 <- priorDrift(drift)
          p2 <- priorDaily(daily)
        } yield p1 + p2
      case _ => None
    }

    // 1. read data from file
    // 2. compose model with particle filter to get function from Parameters => LogLikelihood
    // 3. Build the PMMH algorithm using the mll estimate from the particle filter
    // 4. write it to a file as a stream
    def iters(chain: Int) = for {
      data <- DataFromFile("data/seasonalPoissonSims.csv").observations.runWith(Sink.seq)
      filter = ParticleFilter.likelihood[Vector](data.sortBy(_.t).toVector, 
        Resampling.treeSystematicResampling, 1500).
      compose(model)
      pmmh = ParticleMetropolis(filter.run, params, Parameters.perturb(0.05), p => prior(p).get)
      io <- pmmh.params.take(100000).map(_.show).
        runWith(Streaming.writeStreamToFile(s"data/seasonalPoissonParams-$chain.csv"))
    } yield io

    Future.sequence((1 to 2).map(iters)).
      onComplete(_ => system.terminate())
  }
}

object PilotRunComposed extends App with TestModel {
  setParallelismGlobally(8) // set the number of threads to use for the parallel particle filter

  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[Vector, Id, State] = Resampling.treeSystematicResampling _

  val res = for {
    data <- DataFromFile("data/seasonalPoissonSims.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun[Vector](data.toVector, model, params, resample, particles)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/ComposedPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}
