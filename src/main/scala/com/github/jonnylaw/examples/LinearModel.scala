package com.github.jonnylaw.examples


import fs2.{io, text, Task}
import breeze.stats.distributions.{Gaussian, Gamma}
import cats.data.Reader
import com.github.jonnylaw.model._
import breeze.linalg.{DenseMatrix, DenseVector}
import cats.implicits._
import java.nio.file.Paths
import java.io.{File, FileWriter, BufferedWriter}

trait LinearModel {
  val p = Parameters.leafParameter(
    Some(1.0),
    SdeParameter.brownianParameter(
      DenseVector(1.0), DenseMatrix((1.0)), DenseVector(-0.2), DenseMatrix((1.0)))
  )

  val mod = Model.linearModel(Sde.brownianMotion)

  def mll(n: Int, data: Vector[Data]) =
    ParticleFilter.likelihood(ParticleFilter.multinomialResampling, data, n) compose mod
}

/**
  * Simulate from the linear model
  */
object SimLinear extends App with LinearModel {
  val writeSims: Task[Unit] = SimulatedData(mod(p)).
    simRegular[Task](1.0).
    take(1000).
    map((x: Data) => x.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/LinearModelSims.csv"))).
    run

  writeSims.unsafeRun()
}

/**
  * Random walk Metropolis Hastings
  */
object LinearModelMCMC extends App with LinearModel {
  // Read in the data as a Vector
  val data: Vector[Data] = io.file.readAll[Task](Paths.get("data/LinearModelSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun

  val prior: Parameters => LogLikelihood = p => { p match {
    case LeafParameter(Some(v), BrownianParameter(m0, c0, mu, sigma)) =>
      Gaussian(0.1, 10.0).logPdf(m0(0)) +
      Gamma(0.1, 10.0).logPdf(1/c0.data(0)) +
      Gamma(1.0, 1.0).logPdf(1/v) +
      Gaussian(-0.2, 10.0).logPdf(mu(0)) +
      Gamma(0.1, 10.0).logPdf(1/sigma.data(0))
  }}
  
  val mll: Reader[Parameters, LogLikelihood] = mll(200, data)

  ParticleMetropolis(mll.run, p, Parameters.perturb(0.05), prior).
    iters[Task].
    take(10000).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/LinearModelPMMH.csv"))).
    run.
    unsafeRun
}
