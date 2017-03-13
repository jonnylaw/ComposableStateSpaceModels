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
  def add(delta: DenseVector[Double]): SdeParameter
  def map(f: Double => Double): SdeParameter
  def toMap: Map[String, Double]
  def m0: DenseVector[Double]
  def c0: DenseVector[Double]
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
      covInit <- MultivariateGaussian(c0, diag(DenseVector.fill(dimension)(delta)))
      m <- MultivariateGaussian(mu, diag(DenseVector.fill(dimension)(delta)))
      s <- MultivariateGaussian(sigma, diag(DenseVector.fill(dimension)(delta)))
    } yield SdeParameter.brownianParameter(meanInit, covInit, m, s)
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val dim = m0.size
    SdeParameter.brownianParameter(
      m0 :+ delta(0 to dim - 1),
      c0 :+ delta(dim to 2 * dim - 1),
      mu :+ delta(2 * dim to 3 * dim - 1),
      sigma :+ delta(3 * dim to -1))
  }

  def flatten: Vector[Double] = m0.data.toVector ++ c0.data.toVector ++ mu.data.toVector ++ sigma.data.toVector

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
      covInit <- MultivariateGaussian(c0, diag(DenseVector.fill(dimension)(delta)))
      t <- MultivariateGaussian(theta, diag(DenseVector.fill(dimension)(delta)))
      a <- MultivariateGaussian(alpha, diag(DenseVector.fill(dimension)(delta)))
      s <- MultivariateGaussian(sigma, diag(DenseVector.fill(dimension)(delta)))
    } yield SdeParameter.ornsteinParameter(meanInit, covInit, t, a, s)
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val dim = m0.size
    SdeParameter.ornsteinParameter(
      m0 :+ delta(0 to dim - 1), 
      c0 :+ delta(dim to 2 * dim - 1),
      theta :+ delta(2 * dim to 3 * dim - 1),
      alpha :+ delta(3 * dim to 4 * dim - 1),
      sigma :+ delta(4 * dim to -1))
  }


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
