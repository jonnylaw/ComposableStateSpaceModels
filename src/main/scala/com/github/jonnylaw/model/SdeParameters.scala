package com.github.jonnylaw.model

import breeze.linalg.{DenseMatrix, DenseVector, diag}
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import scala.util.{Try, Success, Failure}
import breeze.numerics.exp

sealed trait SdeParameter {
  def flatten: Vector[Double]
  def length: Int = this.flatten.length
  def sum(that: SdeParameter): Try[SdeParameter]
  def perturb(delta: Double): Rand[SdeParameter]
  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter]
}

case class BrownianParameter(
  m0: DenseVector[Double],
  c0: DenseMatrix[Double],
  mu: DenseVector[Double],
  sigma: DenseMatrix[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case BrownianParameter(m01, c01, m1, c1) =>
      Success(SdeParameter.brownianParameter(m0 + m01, c0 + c01, mu + m1, sigma + c1))
    case _ => Failure(throw new Exception(s"Can't sum Brownianparameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    val dimension = m0.size
    for {
      meanInit <- MultivariateGaussian(m0, diag(DenseVector.fill(dimension)(delta)))
      cInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      covInit = diag(c0) :* exp(cInnov)
      m <- MultivariateGaussian(mu, diag(DenseVector.fill(dimension)(delta)))
      sInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      s = diag(sigma) :* exp(sInnov)
    } yield SdeParameter.brownianParameter(meanInit, diag(covInit), m, diag(s))
  }

  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter] = ???

  def flatten: Vector[Double] =
    mu.data.toVector ++ diag(sigma).data.toVector
}

case class OrnsteinParameter(
  m0: DenseVector[Double],
  c0: DenseMatrix[Double],
  theta: DenseVector[Double],
  alpha: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case OrnsteinParameter(m, c, t, a, s) =>
      Success(SdeParameter.ornsteinParameter(m0 + m, c0 + c, theta + t, alpha + a, sigma + s))
    case _ => Failure(throw new Exception(s"Can't sum OrnsteinParameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    val dimension = m0.size
    for {
      meanInit <- MultivariateGaussian(m0, diag(DenseVector.fill(dimension)(delta)))
      cInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      covInit = diag(c0) :* exp(cInnov)
      t <- MultivariateGaussian(theta, diag(DenseVector.fill(dimension)(delta)))
      aInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      a = alpha :* exp(aInnov)
      sInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      s = sigma :* exp(sInnov)
    } yield SdeParameter.ornsteinParameter(meanInit, diag(covInit), t, a, s)
  }

  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter] = ???
  //     innov <- MultivariateGaussian(
  //       DenseVector.zeros[Double](3 * theta.length),
  //       diag(DenseVector.fill(3 * theta.length)(delta)))
  //     t = theta + innov(0 to theta.length - 1)
  //     a = alpha :* exp(innov(theta.length to 2 * theta.length - 1))
  //     s = sigma :* exp(innov(2 * theta.length to -1))
  //   } yield SdeParameter.ornsteinParameter(t, a, s)
  // }

  def flatten: Vector[Double] =
    theta.data.toVector ++ alpha.data.toVector ++ sigma.data.toVector
}

object SdeParameter {
  // smart constructors
  def brownianParameter(
    m0: DenseVector[Double],
    c0: DenseMatrix[Double],
    mu: DenseVector[Double],
    sigma: DenseMatrix[Double]): SdeParameter = {

    BrownianParameter(m0, c0, mu, sigma)
  }

  def ornsteinParameter(
    m0: DenseVector[Double],
    c0: DenseMatrix[Double],
    theta: DenseVector[Double],
    alpha: DenseVector[Double],
    sigma: DenseVector[Double]): SdeParameter = {

    OrnsteinParameter(m0, c0, theta, alpha, sigma)
  }
}
