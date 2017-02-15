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

trait SeasonalTestModel {
  val seasonalParam: Parameters = Parameters.leafParameter(
    Some(3.0),
    SdeParameter.brownianParameter(
      m0 = DenseVector.fill(6)(0.5),
      c0 = DenseVector.fill(6)(0.12),
      mu = DenseVector.fill(6)(0.1),
      sigma = DenseVector.fill(6)(0.5))
  )

  val mod = Model.seasonalModel(24, 3, Sde.brownianMotion)
}

object SimSeasonalModel extends App with SeasonalTestModel {
  SimulatedData(mod(seasonalParam)).
    simRegular[Task](1.0).
    take(200).
    map((d: Data) => d.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/SeasonalModelSims.csv"))).
    run.
    unsafeRun()
}

object FilterSeasonal extends App with SeasonalTestModel {
  val data = io.file.readAll[Task](Paths.get("data/SeasonalModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(",")).
    take(200).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

  val t0 = 0.0

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.systematicResampling, t0, 1000)

  // I think there is something wrong with getIntervals

  data.
    through(filter(mod(seasonalParam))).
    map(ParticleFilter.getIntervals(mod(seasonalParam))).
    drop(1).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/SeasonalModelFiltered.csv"))).
    run.
    unsafeRun()
}
