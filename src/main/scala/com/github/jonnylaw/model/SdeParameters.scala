package com.github.jonnylaw.model

import breeze.linalg.{DenseMatrix, DenseVector, diag}
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import cats._
import cats.implicits._
import scala.util.{Try, Success, Failure}
import breeze.numerics.exp

sealed trait SdeParameter {
  def sum(that: SdeParameter): Try[SdeParameter]
  def perturb(delta: Double): Rand[SdeParameter]
  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter]
}
case class BrownianParameter(
  mu: DenseVector[Double],
  sigma: DenseMatrix[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case BrownianParameter(m1, c1) =>
      Success(SdeParameter.brownianParameter(mu + m1, sigma + c1))
    case _ => Failure(throw new Exception(s"Can't sum Brownianparameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    for {
      innov <- MultivariateGaussian(
        DenseVector.zeros[Double](2 * mu.length),
        diag(DenseVector.fill(2 * mu.length)(delta)))
      m = mu + innov(0 to mu.length - 1)
      s = diag(sigma) :* exp(innov(mu.length to -1))
    } yield SdeParameter.brownianParameter(m, diag(s))
  }

  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter] = ???
}

object BrownianParameter {
  def apply(mu: Double, sigma: Double): BrownianParameter = {
    BrownianParameter(DenseVector(mu), DenseMatrix(sigma))
  }
}

case class OrnsteinParameter(
  theta: DenseVector[Double],
  alpha: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case OrnsteinParameter(t, a, s) =>
      Success(SdeParameter.ornsteinParameter(theta + t, alpha + a, sigma + s))
    case _ => Failure(throw new Exception(s"Can't sum OrnsteinParameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    for {
      innov <- MultivariateGaussian(
        DenseVector.zeros[Double](3 * theta.length),
        diag(DenseVector.fill(3 * theta.length)(delta)))
      t = theta + innov(0 to theta.length - 1)
      a = alpha :* exp(innov(theta.length to 2 * theta.length - 1))
      s = sigma :* exp(innov(2 * theta.length to -1))
    } yield SdeParameter.ornsteinParameter(t, a, s)
  }

  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter] = ???
}

case class StepConstantParameter(a: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case StepConstantParameter(b) =>
      Success(SdeParameter.stepConstantParameter(a + b))
    case _ => Failure(throw new Exception(s"Can't sum StepConstantParameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    for {
      a <- MultivariateGaussian(a, diag(DenseVector.fill(a.length)(delta)))
    } yield SdeParameter.stepConstantParameter(a)
  }

  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter] = ???

}

object SdeParameter {
  // smart constructors
  def brownianParameter(
    mu: DenseVector[Double],
    sigma: DenseMatrix[Double]): SdeParameter = {

    BrownianParameter(mu, sigma)
  }

  def ornsteinParameter(
    theta: DenseVector[Double],
    alpha: DenseVector[Double],
    sigma: DenseVector[Double]): SdeParameter = {

    OrnsteinParameter(theta, alpha, sigma)
  }

  def stepConstantParameter(a: DenseVector[Double]): SdeParameter = {
    StepConstantParameter(a)
  }
}
