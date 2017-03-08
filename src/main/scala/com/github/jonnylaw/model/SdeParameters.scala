package com.github.jonnylaw.model

import breeze.linalg.{DenseVector, diag}
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import scala.util.{Try, Success, Failure}
import breeze.numerics.exp

sealed trait SdeParameter {
  def flatten: Vector[Double]
  def length: Int = this.flatten.length
  def sum(that: SdeParameter): Try[SdeParameter]
  def perturb(delta: Double): Rand[SdeParameter]
  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter]
  def map(f: Double => Double): SdeParameter
  def toMap: Map[String, Double]
}

case class BrownianParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
  mu: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

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
        diag(DenseVector.fill(dimension)(delta))
      )
      covInit = c0 :* exp(cInnov)
      m <- MultivariateGaussian(mu, diag(DenseVector.fill(dimension)(delta)))
      sInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta))
      )
      s = sigma :* exp(sInnov)
    } yield SdeParameter.brownianParameter(meanInit, covInit, m, s)
  }

  def perturbIndep(delta: Vector[Double]): Rand[SdeParameter] = ???

  def flatten: Vector[Double] =
    m0.data.toVector ++ c0.data.toVector ++ mu.data.toVector ++ sigma.data.toVector

  def map(f: Double => Double): SdeParameter = {
    SdeParameter.brownianParameter(m0.mapValues(f), c0.mapValues(f), mu.mapValues(f), sigma.mapValues(f))
  }

  def toMap: Map[String, Double] = {
    SdeParameter.denseVectorMap(m0, "m0") ++ SdeParameter.denseVectorMap(c0, "c0") ++
      SdeParameter.denseVectorMap(mu, "mu") ++ SdeParameter.denseVectorMap(sigma, "sigma")
  }
}

case class OrnsteinParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
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
      covInit = c0 :* exp(cInnov)
      t <- MultivariateGaussian(theta, diag(DenseVector.fill(dimension)(delta)))
      aInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      a = alpha :* exp(aInnov)
      sInnov <- MultivariateGaussian(
        DenseVector.zeros[Double](dimension),
        diag(DenseVector.fill(dimension)(delta)))
      s = sigma :* exp(sInnov)
    } yield SdeParameter.ornsteinParameter(meanInit, covInit, t, a, s)
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
    m0.data.toVector ++ c0.data.toVector ++ theta.data.toVector ++ alpha.data.toVector ++ sigma.data.toVector

  def map(f: Double => Double): SdeParameter = {
    SdeParameter.ornsteinParameter(m0.mapValues(f), c0.mapValues(f), theta.mapValues(f), alpha.mapValues(f), sigma.mapValues(f))
  }

  def toMap: Map[String, Double] = {
    SdeParameter.denseVectorMap(m0, "m0") ++ SdeParameter.denseVectorMap(c0, "c0") ++
      SdeParameter.denseVectorMap(theta, "theta") ++ SdeParameter.denseVectorMap(alpha, "alpha") ++
      SdeParameter.denseVectorMap(sigma, "sigma")
  }
}

object SdeParameter {
  // smart constructors
  def brownianParameter(
    m0: DenseVector[Double],
    c0: DenseVector[Double],
    mu: DenseVector[Double],
    sigma: DenseVector[Double]): SdeParameter = {

    BrownianParameter(m0, c0, mu, sigma)
  }

  def ornsteinParameter(
    m0: DenseVector[Double],
    c0: DenseVector[Double],
    theta: DenseVector[Double],
    alpha: DenseVector[Double],
    sigma: DenseVector[Double]): SdeParameter = {

    OrnsteinParameter(m0, c0, theta, alpha, sigma)
  }

  def denseVectorMap(s: DenseVector[Double], name: String): Map[String, Double] = {
    s.data.zipWithIndex.
      map { case (value, i) => (name + "_" + i -> value) }.
      foldLeft(Map[String, Double]())((acc, a) => acc + a)
  }
}
