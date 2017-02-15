package com.github.jonnylaw.examples

import java.nio.file.Paths
import com.github.jonnylaw.model._
import breeze.stats.distributions.{Gaussian, MultivariateGaussian, Gamma}
import breeze.linalg.{DenseVector, diag}
import cats.implicits._
import fs2._

/**
  * Simulate an SDE
  */
object SimulateBrownianMotion extends App {
  val p = SdeParameter.brownianParameter(
    DenseVector.fill(2)(1.0),
    DenseVector.fill(2)(1.0),
    DenseVector.fill(2)(-0.3),
    DenseVector.fill(2)(0.3))

  val p1 = SdeParameter.brownianParameter(
    DenseVector.fill(2)(1.0),
    DenseVector.fill(2)(1.0),
    DenseVector.fill(2)(0.3),
    DenseVector.fill(2)(0.3))

  val sde1 = Sde.brownianMotion(p)
  val sde2 = Sde.brownianMotion(p1)

  val composedSde = sde1 |+| sde2

  composedSde.
    simStream[Task](0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/brownianMotion.csv"))).
    run.
    unsafeRun
}

object SimOrnstein extends App {
  val p = SdeParameter.ornsteinParameter(
    DenseVector.fill(2)(0.0),
    DenseVector.fill(2)(3.0),
    theta = DenseVector.fill(2)(2.0), 
    alpha = DenseVector.fill(2)(0.5),
    sigma = DenseVector.fill(2)(0.3))

  val sde = Sde.ornsteinUhlenbeck

  sde(p).
    simStream[Task](0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/ornsteinUhlenbeck.csv"))).
    run.
    unsafeRun
}
