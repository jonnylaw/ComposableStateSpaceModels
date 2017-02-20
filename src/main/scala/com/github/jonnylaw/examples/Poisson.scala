package com.github.jonnylaw.examples

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import fs2._
import java.nio.file.Paths
import scala.collection.parallel.immutable.ParVector

trait PoissonTestModel {
  val poissonParam: Parameters = Parameters.leafParameter(
    None,
    SdeParameter.brownianParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      mu = DenseVector(0.1),
      sigma = DenseVector(0.5))
  )

  val mod = Model.poissonModel(Sde.brownianMotion)
}

object SimPoissonModel extends App with PoissonTestModel {
  SimulatedData(mod(poissonParam)).
    observations[Task].
    take(200).
    map((d: Data) => d.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/PoissonModelSims.csv"))).
    run.
    unsafeRun()
}

object FilterPoisson extends App with PoissonTestModel {
  val data = io.file.readAll[Task](Paths.get("data/PoissonModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

  val t0 = 0.0

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.systematicResampling, t0, 1000)

  data.
    through(filter(mod(poissonParam))).
//   map(s => ParticleFilter.getMeanForecast(s, mod(poissonParam), s.t + 1.0)).
    map(ParticleFilter.getIntervals(mod(poissonParam))).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/PoissonModelFiltered.csv"))).
    run.
    unsafeRun()
}

object ForecastPoisson extends App with PoissonTestModel {
  val data = io.file.readAll[Task](Paths.get("data/PoissonModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

  val t0 = 0.0

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.systematicResampling, t0, 1000)

  data.
    through(filter(mod(poissonParam))).
    map(s => ParticleFilter.getMeanForecast(s, mod(poissonParam), s.t + 0.1)).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/PoissonModelForecast.csv"))).
    run.
    unsafeRun()
}

object PilotRunPoisson extends App with PoissonTestModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  val data = io.file.readAll[Task](Paths.get("data/PoissonModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun.
    sortBy(_.t)

  val mll = (n: Int) => ParticleFilter.parLikelihood(data.toVector, n).compose(mod)

  val proposal = (p: Parameters) => Rand.always(p)

  val prior = (p: Parameters) => 0.0

  def iters(n: Int): Task[Double] = {
    val its = ParticleMetropolis(mll(n).run, poissonParam, proposal, prior).
      iters[Task].
      take(100).
      runLog

    its map (s => breeze.stats.variance(s.map(_.ll)))
  }

  val particles = Vector(100, 200, 500, 1000, 2000)

  val variances = Task.parallelTraverse(particles)(iters).
    unsafeRun

  Stream.emits[Task, (Double, Int)](variances.zip(particles)).
    map{ case (v, n) => s"$n, $v" }.
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/PoissonPilotRun.csv"))).
    run.
    unsafeRun()
}

object DeterminePoissonParams extends App with PoissonTestModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  // read in the data
  val data = io.file.readAll[Task](Paths.get("data/PoissonModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun.
    sortBy(_.t)

  // Choose n based on the pilot run, a rule of thumb is the variance of the log of their
  // estimated marginal likelihood is equal to one
  val n = 500

  // determine parameters
  val mll = ParticleFilter.parLikelihood(data.toVector, n).compose(mod)

  val prior = (p: Parameters) => p match {
    case LeafParameter(None, BrownianParameter(m, s, mu, sigma)) =>
      Gaussian(0.5, 3.0).logPdf(m(0)) +
      Gamma(1.0, 1.0).logPdf(1/s(0)) + 
      Gaussian(0.5, 3.0).logPdf(mu(0)) +
      Gamma(1.0, 2.0).logPdf(1/sigma(0))
  }

  def iters(chain: Int) = {
    ParticleMetropolis(mll.run, poissonParam, Parameters.perturb(0.05), prior).
      iters[Task].
      take(10000).
      map(_.show).
      intersperse("\n").
      through(text.utf8Encode).
      through(io.file.writeAll[Task](Paths.get(s"data/PoissonParams-Chain-$chain.csv"))).
      run
  }

  Task.parallelTraverse(1 to 2)(iters).
    unsafeRun()
}
