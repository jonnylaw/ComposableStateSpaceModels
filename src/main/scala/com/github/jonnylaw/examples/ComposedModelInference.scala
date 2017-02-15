package com.github.jonnylaw.examples

import breeze.linalg.DenseVector
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import cats.data.Reader
import cats.implicits._

import com.github.jonnylaw.model._
import Parameters._

import fs2._

import java.nio.file.Paths
import java.io.PrintWriter

/**
  * Define a model to use throughout the examples in this file
  */
trait TestModel {
  val poissonParams = Parameters.leafParameter(
    None,
    SdeParameter.brownianParameter(
      DenseVector(1.0), 
      DenseVector(1.0), 
      DenseVector(0.01), 
      DenseVector(0.3)))

  val seasonalParams = Parameters.leafParameter(
    Some(1.0),
    SdeParameter.brownianParameter(
      DenseVector.fill(6)(1.0),
      DenseVector.fill(6)(1.0),
      DenseVector.vertcat(DenseVector.fill(2)(0.01),
        DenseVector.fill(2)(-0.05),
        DenseVector.fill(2)(0.05)),
      DenseVector.fill(6)(0.3)))

  val params = poissonParams |+| seasonalParams
  val poisson = Model.poissonModel(Sde.brownianMotion)
  val seasonal = Model.seasonalModel(24, 3, Sde.brownianMotion)
  val model = poisson |+| seasonal
}


/**
  * Simulate a poisson model, with seasonal rate parameter
  */
object SimulateSeasonalPoisson extends App with TestModel {
  val times = (0.0 to 120.0 by 1.0).
    filter(a => scala.util.Random.nextDouble < 0.9). // 10% of data missing
    toList

  Stream.emits[Task, Double](times).
    through(SimulatedData(model(params)).simPompModel(0.0)).
    map((x: Data) => x.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/seasonalPoissonSims.csv"))).
    run.
    unsafeRun
}

object FilterSeasonalPoisson extends App with TestModel {
  val n = 1000 // number of particles for the particle filter
  val t0 = 0.0 // starting time of the filter (time of the first observation)
  val filter = ParticleFilter.filter[List](ParticleFilter.systematicResampling, 0.0, n)

  io.file.readAll[Task](Paths.get("data/seasonalPoissonSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    through(filter(model(params))).
    map(ParticleFilter.getIntervals(model(params))).
    drop(1).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/seasonalPoissonFiltered.csv"))).
    run.
    unsafeRun
}

object DetermineComposedParams extends App with TestModel {
  // read a file from memory
  val data = scala.io.Source.fromFile("data/seasonalPoissonSims.csv").
    getLines.
    toList.
    take(120).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))


  def priorDrift(p: Parameters) = p match {
    case LeafParameter(_, BrownianParameter(m0, c0, mu, sigma)) =>
      Some(
        Gaussian(15.0, 10.0).logPdf(m0(0)) +
        Gamma(1.0, 8.0).logPdf(1/c0(0)) +
        Gaussian(0.0, 10.0).logPdf(mu(0)) +
        Gamma(0.1, 10.0).logPdf(1/sigma(0)))
    case _ => None
  }

  def priorDaily(p: Parameters) = p match {
    case LeafParameter(_, OrnsteinParameter(m0, c0, theta, alpha, sigma)) =>
      Some(
        m0.mapValues(m => Gaussian(-0.5, 10.0).logPdf(m)).reduce(_+_) +
        c0.mapValues(c => Gamma(1.0, 10.0).logPdf(1/c)).reduce(_+_) +
        theta.mapValues(m => Gaussian(-1.0, 10.0).logPdf(m)).reduce(_+_) +
        alpha.mapValues(a => Gaussian(0.1, 10.0).logPdf(a)).reduce(_+_) +
        sigma.mapValues(s => Gamma(0.5, 6.0).logPdf(s)).reduce(_+_))
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

  // determine parameters
  val mll: Reader[Parameters, LogLikelihood] = 
    ParticleFilter.parLikelihood(data.sortBy(_.t), 500) compose model

  val iters = ParticleMetropolis(mll.run, params, Parameters.perturb(0.05), p => prior(p).get).
    markovIters.
    steps.
    take(10000)

  val pw = new PrintWriter("data/SeasonalPoissonParams.csv")

  for (iter <- iters) {
    pw.write(iter + "\n")
  }

  pw.close()
}

object PilotRunComposed extends App with TestModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  val data = io.file.readAll[Task](Paths.get("data/seasonalPoissonSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(120).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun.
    sortBy(_.t)

  val t0 = 0.0

  // determine parameters
  val mll = (n: Int) => ParticleFilter.parLikelihood(data.toList, n).compose(model)

  val proposal = (p: Parameters) => Rand.always(p)

  val prior = (p: Parameters) => 0.0

  def iters(n: Int): Task[Double] = {
    val its = ParticleMetropolis(mll(n).run, params, proposal, prior).
      iters[Task].
      take(100).
      runLog

    its map (s => breeze.stats.variance(s.map(_.ll)))
  }

  val particles = List(100, 200, 500, 1000, 2000)

  val variances = Task.parallelTraverse(particles)(iters).
    unsafeRun

  import java.io.PrintWriter
  val pw = new PrintWriter("data/ComposedPilotRun.csv")
  pw.write("Particles, mll Variance")
  pw.write(particles.zip(variances).mkString(", "))
  pw.close
}
