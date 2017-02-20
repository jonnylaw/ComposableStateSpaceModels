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

import scala.collection.parallel.immutable.ParVector

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
      DenseVector(0.1)))

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
}

/**
  * Simulate a poisson model, with seasonal rate parameter
  */
object SimulateSeasonalPoisson extends App with TestModel {
  val times = (0.0 to 240.0 by 1.0).
    filter(a => scala.util.Random.nextDouble < 0.9). // 10% of data missing
    toList

  Stream.emits[Task, Double](times).
    through(SimulatedData(model(params)).simPompModel(0.0)).
    map((x: Data) => x.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/seasonalPoissonSims.csv"))).
    run.
    unsafeRun()
}

object FilterSeasonalPoisson extends TestModel {
  def main(args: Array[String]): Unit = {
    val n = 1000 // number of particles for the particle filter
    val t0 = 0.0 // starting time of the filter (time of the first observation)
    setParallelismGlobally(8) // set the number of threads to use for the parallel particle filter
    val filter = ParticleFilter.filter[ParVector](ParticleFilter.systematicResampling, 0.0, n)

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
      unsafeRun()
  }
}

object DetermineComposedParams extends TestModel {
  def main(args: Array[String]) = {
    val nParticles = args.head.toInt
    // read a file from memory
    val data = scala.io.Source.fromFile("data/seasonalPoissonSims.csv").
      getLines.
      toVector.
      take(120).
      map(a => a.split(",")).
      map(d => TimedObservation(d(0).toDouble, d(1).toDouble))


    def priorDrift(p: Parameters) = p match {
      case LeafParameter(_, BrownianParameter(m0, c0, mu, sigma)) =>
        Some(
          Gaussian(1.0, 3.0).logPdf(m0(0)) +
            Gamma(0.5, 2.0).logPdf(1/c0(0)) +
            Gaussian(0.0, 3.0).logPdf(mu(0)) +
            Gamma(0.1, 3.0).logPdf(1/sigma(0)))
      case _ => None
    }

    def priorDaily(p: Parameters) = p match {
      case LeafParameter(_, OrnsteinParameter(m0, c0, theta, alpha, sigma)) =>
        Some(
          m0.mapValues(m => Gaussian(-0.5, 3.0).logPdf(m)).reduce(_+_) +
            c0.mapValues(c => Gamma(0.3, 3.0).logPdf(1/c)).reduce(_+_) +
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

    setParallelismGlobally(8) // set the number of threads to use for the parallel particle filter

    // compose the particle filter with the model to get a function from Parameters => LogLikelihood
    val mll = ParticleFilter.parLikelihood(data.sortBy(_.t), nParticles) compose model

    ParticleMetropolis(mll.run, params, Parameters.perturb(0.05), p => prior(p).get).
      iters[Task].
      take(10000).
      map(_.show).
      intersperse("\n").
      through(text.utf8Encode).
      through(io.file.writeAll(Paths.get("data/SeasonalPoissonParams.csv"))).
      run.
      unsafeRun()
  }
}

object PilotRunComposed extends App with TestModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")
  setParallelismGlobally(8) // set the number of threads to use for the parallel particle filter

  val data = io.file.readAll[Task](Paths.get("data/seasonalPoissonSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(1000).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun.
    sortBy(_.t)

  val t0 = 0.0

  // determine parameters
  val mll = (n: Int) => ParticleFilter.parLikelihood(data.toVector, n).compose(model)

  val proposal = (p: Parameters) => Rand.always(p)

  val prior = (p: Parameters) => 0.0

  def varianceMll(n: Int): Task[Double] = {
    val lls = ParticleMetropolis(mll(n).run, params, proposal, prior).
      iters[Task].
      take(100).
      map(_.ll).
      runLog

    lls map (x => breeze.stats.variance(x))
  }

  val particles = Vector(100, 200, 500, 1000, 2000)

  val pw = new PrintWriter("data/ComposedPilotRun.csv")
  pw.write("particles, mll_variance")

  Task.parallelTraverse(particles)(varianceMll).
    unsafeRun().
    zip(particles).
    foreach{ case (v, part) => pw.write(s"\n$part, $v") }

  pw.close()
}
