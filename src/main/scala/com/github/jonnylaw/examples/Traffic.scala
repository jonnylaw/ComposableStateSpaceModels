package com.github.jonnylaw.examples

import breeze.linalg.DenseVector
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import fs2._
import java.nio.file.Paths
import scala.collection.parallel.immutable.ParVector

trait TrafficModel {
  // define the model
  val poissonParam: Parameters = Parameters.leafParameter(
    None,
    SdeParameter.brownianParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      mu = DenseVector(0.0),
      sigma = DenseVector(0.01)))

  val seasonalParamDaily = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(
        -0.67, -0.53,
        -0.83, -0.58,
        -0.57, -0.59),
      c0 = DenseVector(
        0.10, 0.15,
        0.11, 0.04,
        0.34, 0.06),
      theta = DenseVector(
        -0.81, -1.01,
        -0.96, -0.22,
        -0.14, -0.40),
      alpha = DenseVector(
        0.06, 0.17,
        0.07, 0.14,
        0.05, 0.10),
      sigma = DenseVector(
        0.22, 0.33,
        0.13, 0.17,
        0.24, 0.27)))

  val seasonalParamWeekly = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(
        -0.67, -0.53,
        -0.83, -0.58,
        -0.57, -0.59),
      c0 = DenseVector(
        0.10, 0.15,
        0.11, 0.04,
        0.34, 0.06),
      theta = DenseVector(
        -0.81, -1.01,
        -0.96, -0.22,
        -0.14, -0.40),
      alpha = DenseVector(
        0.06, 0.17,
        0.07, 0.14,
        0.05, 0.10),
      sigma = DenseVector(
        0.22, 0.33,
        0.13, 0.17,
        0.24, 0.27)))

  val weeklyParams = poissonParam |+| seasonalParamDaily |+| seasonalParamWeekly

  val drift = Model.poissonModel(Sde.brownianMotion)
  val daily = Model.seasonalModel(24, 3, Sde.ornsteinUhlenbeck)

  val weekly = Model.seasonalModel(24*7, 3, Sde.ornsteinUhlenbeck)
  val weeklyMod = drift |+| daily |+| weekly

  val dailyParams = poissonParam |+| seasonalParamDaily
  val dailyMod = drift |+| daily

  // construct a prior on the parameters
  def priorDrift(p: Parameters) = p match {
    case LeafParameter(_, BrownianParameter(m0, c0, mu, sigma)) =>
      Gaussian(15.0, 10.0).logPdf(m0(0)) + Gamma(1.0, 8.0).logPdf(1/c0(0)) +
      Gaussian(0.0, 10.0).logPdf(mu(0)) + Gamma(0.1, 10.0).logPdf(1/sigma(0))
    case _ => throw new Exception
  }

  def priorDaily(p: Parameters) = p match {
    case LeafParameter(_, OrnsteinParameter(m0, c0, theta, alpha, sigma)) =>
      m0.mapValues(m => Gaussian(-0.5, 10.0).logPdf(m)).reduce(_+_) + 
      c0.mapValues(c => Gamma(1.0, 10.0).logPdf(1/c)).reduce(_+_) +
      theta.mapValues(m => Gaussian(-1.0, 10.0).logPdf(m)).reduce(_+_) +
      alpha.mapValues(a => Gaussian(0.1, 10.0).logPdf(a)).reduce(_+_) +
      sigma.mapValues(s => Gamma(0.5, 6.0).logPdf(s)).reduce(_+_)
    case _ => throw new Exception
  }

  def priorWeekly(p: Parameters) = p match {
    case LeafParameter(_, OrnsteinParameter(m0, c0, theta, alpha, sigma)) =>
      m0.mapValues(m => Gaussian(-0.5, 10.0).logPdf(m)).reduce(_+_) + 
      c0.mapValues(c => Gamma(1.0, 10.0).logPdf(1/c)).reduce(_+_) +
      theta.mapValues(m => Gaussian(-1.0, 10.0).logPdf(m)).reduce(_+_) +
      alpha.mapValues(a => Gaussian(0.1, 10.0).logPdf(a)).reduce(_+_) +
      sigma.mapValues(s => Gamma(0.5, 6.0).logPdf(s)).reduce(_+_)
    case _ => throw new Exception
  }

  def weeklyPrior = (p: Parameters) => p match {
    case BranchParameter(BranchParameter(drift, daily), weekly) =>
      priorDrift(drift) + priorDaily(daily) + priorWeekly(weekly)
    case _ => throw new Exception
  }

  def dailyPrior = (p: Parameters) => p match {
    case BranchParameter(drift, daily) =>
      priorDrift(drift) + priorDaily(daily)
    case _ => throw new Exception
  }

}

object TrafficModelParams extends App with TrafficModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  // read the traffic data
  val data: Vector[Data] = io.file.readAll[Task](Paths.get("data/TrainingTraffic.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(5945).
    zipWithIndex.
    filter { case (_, i) => i % 10 == 0 }. // only look at every 10th observation
    map { case (d,_) => d }.
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)). // column one contains relative hours from the first observation
    runLog.
    unsafeRun

  // construct the parallel likelihood
  val mll = ParticleFilter.parLikelihood(data.toList, 1000).
    compose(weeklyMod)

  // Construct a task which determines the parameters of the traffic model
  def iters(chain: Int) = ParticleMetropolis(mll.run, weeklyParams, Parameters.perturb(0.05), weeklyPrior).
    iters[Task].
    take(100000).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/TrafficParamsParallel-$chain.csv"))).
    run

  // run two different MCMC chains
  Task.parallelTraverse((1 to 2))(iters).
    unsafeRun()
}

object TrafficModelDailyParams extends App with TrafficModel {
  // read the traffic data
  val data: Vector[Data] = io.file.readAll[Task](Paths.get("data/TrainingTraffic.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(5945).
    zipWithIndex.
    filter { case (_, i) => i % 10 == 0 }. // only look at every 10th observation
    map { case (d,_) => d }.
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)). // column one contains relative hours from the first observation
    runLog.
    unsafeRun

  // construct the parallel likelihood
  val mll = ParticleFilter.parLikelihood(data.toList, 1000).
    compose(dailyMod)

  // Construct a task which determines the parameters of the traffic model
  ParticleMetropolis(mll.run, dailyParams, Parameters.perturb(0.05), dailyPrior).
    iters[Task].
    take(100000).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/TrafficParamsDaily.csv"))).
    run.
    unsafeRun()
}

/**
  * Run the particle filter with different numbers of particles
  */
object TrafficPilotRun extends App with TrafficModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  // read the traffic data
  val data: Vector[Data] = io.file.readAll[Task](Paths.get("data/TrainingTraffic.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(5945).
    zipWithIndex.
    filter { case (_, i) => i % 10 == 0 }. // only look at every 10th observation
    map { case (d,_) => d }.
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)). // column one contains relative hours from the first observation
    runLog.
    unsafeRun

  // the proposal must be the identity
  def identity = (p: Parameters) => Rand.always(p)

  // a list containing the number of particles to consider
  // 139000.26594866606, 100
  // 95524.53163418142,  200
  // 15745.295290444634, 500
  // 29178.453794769353, 1000
  // 24254.9570483469,   2000

  def particles = List(100, 200, 500, 1000, 2000)

  def mll = (n: Int) => ParticleFilter.parLikelihood(data.toList, n).
    compose(weeklyMod).run

  def variance(n: Int): Task[Double] = {
    val iters: Task[Vector[Double]] = ParticleMetropolis(mll(n), weeklyParams, identity, weeklyPrior).
      iters[Task].
      take(1000).
      map(_.ll).
      runLog

    iters map (i => breeze.stats.variance(i))
  }

  val res: Task[Vector[Double]] = Task.parallelTraverse(particles)(variance)

  res.
    unsafeRun.
    foreach(println)
}

/**
  * Filter the traffic model
  */
object FilterTrafficModel extends App with TrafficModel {
  val data = io.file.readAll[Task](Paths.get("data/TestTraffic.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    take(1867).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun().
    sortBy(_.t)

  // find the starting time of the data
  val t0 = data.map(_.t).min

  val mod = dailyMod
  val p = dailyParams

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.systematicResampling, t0, 1000).
    compose(mod)

  Stream.emits[Task, Data](data).
    through(filter(p)).
    map((s: PfState[ParVector]) => (ParticleFilter.getIntervals(mod(p))(implicitly[Collection[ParVector]])(s), s.ess)).
    map { case (s: PfOut, ess) => s.show + s", $ess" }.
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/TrafficFiltered.csv"))).
    run.
    unsafeRun
}

/**
  * One step forecast Traffic Model
  */
object ForecastTrafficModel extends App with TrafficModel {
    // find the starting time of the data
  val t0 = 588.95

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.multinomialResampling, t0, 5000).
    compose(weeklyMod)

  val dt = 5.0/60 // predict five minutes ahead from each observation

  io.file.readAll[Task](Paths.get("data/TestTraffic.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    through(filter(weeklyParams)).
    map(s => ParticleFilter.getMeanForecast(s, weeklyMod(weeklyParams), s.t + dt)).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/TrafficForecast.csv"))).
    run.
    unsafeRun
}
