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
}

case class GenBrownianParameter(
  m0: Double,
  c0: Double,
  mu: Double,
  sigma: Double) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case GenBrownianParameter(m01, c01, m1, c1) =>
      Success(SdeParameter.genBrownianParameter(m0 + m01, c0 + c01, mu + m1, sigma + c1))
    case _ => Failure(throw new Exception(s"Can't sum Brownianparameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    for {
      meanInit <- Gaussian(m0, delta)
      covInit <- Gaussian(c0, delta)
      m <- Gaussian(mu, delta)
      s <- Gaussian(sigma, delta)
    } yield SdeParameter.genBrownianParameter(meanInit, covInit, m, s)
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    SdeParameter.genBrownianParameter(
      m0 + delta(0),
      c0 + delta(1),
      mu + delta(2),
      sigma + delta(3))
  }

  def flatten: Vector[Double] = Vector(m0, c0, mu, sigma)

  def map(f: Double => Double): SdeParameter = {
    SdeParameter.genBrownianParameter(f(m0), f(c0), f(mu), f(sigma))
  }

  def toMap: Map[String, Double] = {
    Map(("m0" -> m0), ("c0" -> c0), ("mu" -> mu), ("sigma" -> sigma))
  }

}

case class BrownianParameter(
  m0: Double,
  c0: Double,
  sigma: Double) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case BrownianParameter(m01, c01, c1) =>
      Success(SdeParameter.brownianParameter(m0 + m01, c0 + c01, sigma + c1))
    case _ => Failure(throw new Exception(s"Can't sum Brownianparameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    for {
      meanInit <- Gaussian(m0, delta)
      covInit <- Gaussian(c0, delta)
      s <- Gaussian(sigma, delta)
    } yield SdeParameter.brownianParameter(meanInit, covInit, s)
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    SdeParameter.brownianParameter(
      m0 + delta(0),
      c0 + delta(1),
      sigma + delta(3))
  }

  def flatten: Vector[Double] = Vector(m0, c0, sigma)

  def map(f: Double => Double): SdeParameter = {
    SdeParameter.brownianParameter(f(m0), f(c0), f(sigma))
  }

  def toMap: Map[String, Double] = {
    Map(("m0" -> m0), ("c0" -> c0), ("sigma" -> sigma))
  }
}

case class OuParameter(
  m0: Double,
  c0: Double,
  alpha: Double,
  sigma: Double,
  theta: Seq[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Try[SdeParameter] = that match {
    case OuParameter(m, c, a, s, t) if t.size == theta.size =>
      Success(SdeParameter.ouParameter(m0 + m, c0 + c, alpha + a, sigma + s)(
        theta.zip(t).map { case (a, b) => a + b }: _*))
    case _ => Failure(throw new Exception(s"Can't sum OrnsteinParameter with $that"))
  }

  def perturb(delta: Double): Rand[SdeParameter] = {
    for {
      meanInit <- Gaussian(m0, delta)
      covInit <- Gaussian(c0, delta)
      a <- Gaussian(alpha, delta)
      s <- Gaussian(sigma, delta)
      tInnov <- Gaussian(0.0, delta)
      t = theta map (_ + tInnov)
    } yield SdeParameter.ouParameter(meanInit, covInit, a, s)(t: _*)
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    SdeParameter.ouParameter(
      m0 + delta(0), 
      c0 + delta(1),
      alpha + delta(2),
      sigma + delta(3))(
      theta.zip(delta(4 to -1).data).map { case (a, b) => a + b }: _*
    )
  }


  def flatten: Vector[Double] =
    Vector(m0, c0, alpha, sigma) ++ theta.toVector

  def map(f: Double => Double): SdeParameter = {
    SdeParameter.ouParameter(f(m0), f(c0), f(alpha), f(sigma))(theta.map(f): _*)
  }

  def toMap: Map[String, Double] = {
    Map(("m0" -> m0), ("c0" -> c0), ("alpha" -> alpha), ("sigma" -> sigma)) ++
      SdeParameter.seqToMap(theta, "theta")
  }
}

object SdeParameter {
  // smart constructors
  def genBrownianParameter(
    m0: Double,
    c0: Double,
    mu: Double,
    sigma: Double): SdeParameter = {

    GenBrownianParameter(m0, c0, mu, sigma)
  }

  def brownianParameter(
    m0: Double,
    c0: Double,
    sigma: Double): SdeParameter = {

    BrownianParameter(m0, c0, sigma)
  }

  def ouParameter(
    m0: Double,
    c0: Double,
    alpha: Double,
    sigma: Double)(
    theta: Double*): SdeParameter = {

    OuParameter(m0, c0, alpha, sigma, theta)
  }

  def seqToMap(s: Seq[Double], name: String): Map[String, Double] = {
    s.zipWithIndex.
      map { case (value, i) => (name + "_" + i -> value) }.
      foldLeft(Map[String, Double]())((acc, a) => acc + a)
  }
}
