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
}

object SimLinearModel extends App with LinearTestModel {
  SimulatedData(mod(linearParam)).
    observations[Task].
    take(200).
    map((d: Data) => d.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/LinearModelSims.csv"))).
    run.
    unsafeRun()
}

object FilterLinear extends App with LinearTestModel {
  val data = io.file.readAll[Task](Paths.get("data/LinearModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

  val t0 = 0.0

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.systematicResampling, t0, 1000)

  data.
    through(filter(mod(linearParam))).
//   map(s => ParticleFilter.getMeanForecast(s, mod(linearParam), s.t + 1.0)).
    map(ParticleFilter.getIntervals(mod(linearParam))).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/LinearModelFiltered.csv"))).
    run.
    unsafeRun()
}

object ForecastLinear extends App with LinearTestModel {
  val data = io.file.readAll[Task](Paths.get("data/LinearModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun().
    sortBy(_.t)

  val filter = ParticleFilter.filter[List](ParticleFilter.systematicResampling, 0.0, 1000).
    compose(mod)

  // set the prediction interval, predict five minutes ahead from each observation
  val dt = 5.0/60

  // A function which accepts an integer and a sample from the posterior distribution of the parameters
  Stream.emits[Task, Data](data).
    through(filter(linearParam)).
    map(s => ParticleFilter.getMeanForecast(s, mod(linearParam), s.t + dt)). // return the log-likelihood of each path
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/LinearModelForecast.csv"))).
    run.
    unsafeRun()
}
