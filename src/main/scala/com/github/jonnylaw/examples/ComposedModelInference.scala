package com.github.jonnylaw.examples

import breeze.linalg.{DenseVector, DenseMatrix}
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
      DenseVector(1.0), DenseMatrix((1.0)), DenseVector(0.01), DenseMatrix((0.3))))
  val seasonalParams = Parameters.leafParameter(
    Some(1.0),
    SdeParameter.brownianParameter(
      DenseVector.fill(6)(1.0),
      DenseMatrix.eye[Double](6),
      DenseVector.fill(6)(0.01),
      DenseMatrix.eye[Double](6) * 0.3))

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
  val filter = ParticleFilter.filter(0.0, n)

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
    take(200).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

  def prior: Parameters => LogLikelihood = p => 0.0

  // determine parameters
  val mll: Reader[Parameters, LogLikelihood] = ParticleFilter.likelihood(
    ParticleFilter.multinomialResampling, data.sortBy(_.t).toVector, 200) compose model

  val iters = ParticleMetropolis(mll.run, params, Parameters.perturb(0.05), prior).
    markovIters.
    steps.
    take(10000)

  val pw = new PrintWriter("data/SeasonalPoissonParams.csv")

  for (iter <- iters) {
    pw.write(iter + "\n")
  }

  pw.close()
}
