package com.github.jonnylaw.examples

import breeze.linalg.{DenseVector, diag, DenseMatrix}
import cats.implicits._
import com.github.jonnylaw.model._
import fs2._
import java.nio.file.Paths

trait TemperatureModel {
  // define the model
  val linearParam: Parameters = Parameters.leafParameter(
    Some(0.0662326780360091),
    SdeParameter.brownianParameter(
      m0 = DenseVector(14.987419437458),
      c0 = DenseMatrix((0.123760233465146)),
      mu = DenseVector(-0.00792688278955239),
      sigma = DenseMatrix((0.678847257028525))))

  val seasonalParamDaily = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(
        -0.675768430356229, -0.535485175700474,
        -0.839863354676339, -0.585465398276856,
        -0.577353725232412, -0.597149665648091),
      c0 = diag(DenseVector(
        0.101688171314269, 0.151715314425034,
        0.111050425853816, 0.0472460213102321,
        0.347601923518614, 0.0659490397918582)),
      theta = DenseVector(
        -0.817854451851972, -1.01286835908201,
        -0.960259509475283, -0.222493704420246,
        -0.140279342626173, -0.406591406745326),
      alpha = DenseVector(
        0.0662760564571053, 0.173348945772469,
        0.0734761800385968, 0.144486171592152,
        0.0552416958030416, 0.102607781197052),
      sigma = DenseVector(
        0.228185104119382, 0.33366219666742,
        0.139685314234609, 0.176597498964931,
        0.245320195111654, 0.272681259330769)))

  val seasonalParamWeekly = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(
        -0.675768430356229, -0.535485175700474,
        -0.839863354676339, -0.585465398276856,
        -0.577353725232412, -0.597149665648091),
      c0 = diag(DenseVector(
        0.101688171314269, 0.151715314425034,
        0.111050425853816, 0.0472460213102321,
        0.347601923518614, 0.0659490397918582)),
      theta = DenseVector(
        -0.817854451851972, -1.01286835908201,
        -0.960259509475283, -0.222493704420246,
        -0.140279342626173, -0.406591406745326),
      alpha = DenseVector(
        0.0662760564571053, 0.173348945772469,
        0.0734761800385968, 0.144486171592152,
        0.0552416958030416, 0.102607781197052),
      sigma = DenseVector(
        0.228185104119382, 0.33366219666742,
        0.139685314234609, 0.176597498964931,
        0.245320195111654, 0.272681259330769)))

  val weeklyParams = linearParam |+| seasonalParamDaily |+| seasonalParamWeekly
  val dailyParams = linearParam |+| seasonalParamDaily

  val drift = Model.linearModel(Sde.brownianMotion)
  val daily = Model.seasonalModel(24, 3, Sde.ornsteinUhlenbeck)
  val weekly = Model.seasonalModel(24*7, 3, Sde.ornsteinUhlenbeck)
  val weeklyMod = drift |+| daily |+| weekly
}

object WeeklyTempModelGetParams extends App with TemperatureModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  // read the temperature data
  val data: Vector[Data] = io.file.readAll[Task](Paths.get("data/TrainingTemperature.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun

  // construct the likelihood
  val mll = ParticleFilter.likelihood(ParticleFilter.multinomialResampling, data, 500).
    compose(weeklyMod)

  // construct a prior on the parameters
  val prior = (p: Parameters) => 0.0

  // Construct a task which determines the parameters of the temperature model
  def iters(chain: Int) = ParticleMetropolis(mll.run, weeklyParams,
    Parameters.perturb(0.05), prior).
    iters[Task].
    take(100000).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/TemperatureParams-$chain.csv"))).
    run

  // run the task four times in parallel, for four different MCMC chains
  Task.parallelTraverse((1 to 4))(iters)
}

object FilterTemperatureModel extends App with TemperatureModel {
  // find the starting time of the data
  val t0 = 0.0

  io.file.readAll[Task](Paths.get("TestTemperature.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    through(ParticleFilter.filter(t0, 5000)(weeklyMod(weeklyParams))).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll[Task](Paths.get(s"data/TemperatureFiltered.csv"))).
    run.
    unsafeRun
}

// object ForecastTemperatureModel extends App with TemperatureModel {
//   val meanParams: Parameters = dailyParams
//   val mod = dailyMod(meanParams)

//   val filter = Filter(dailyMod, ParticleFilter.multinomialResampling)

//   // calculate last filtered state using the parameters
//   val filtered = filter.accFilter(data, data.map(_.t).min)(1000)(meanParams)
//   val lastState = ParticleFilter.multinomialResampling(filtered.last.particles, filtered.last.weights)

//   val times: Vector[Time] =     io.file.readAll[Task](Paths.get("data/TestTemperature.csv")).
//     through(text.utf8Decode).
//     through(text.lines).
//     map(a => a.split(", ")).
//     map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
//     runLog.
//     unsafeRun

//   Source(times).via(forecastFlow(lastState, times.head, mod)).
//     map(f => ByteString(s"$f\n") ).
//     runWith(FileIO.toPath(Paths.get(s"ForecastTemperature.csv"))).
//     onComplete(_ => system.terminate)
// }
