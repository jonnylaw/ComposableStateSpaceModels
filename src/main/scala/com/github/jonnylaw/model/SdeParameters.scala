package com.github.jonnylaw.model

import breeze.linalg.{DenseVector, diag}
import breeze.stats.distributions._
import breeze.numerics.{exp, sqrt}
import scala.language.implicitConversions
import SdeParameter._

sealed trait SdeParameter { self =>
  def flatten: Seq[Double]
  def length: Int = this.flatten.length
  def sum(that: SdeParameter): Error[SdeParameter]
  def perturb(delta: Double)(implicit rand: RandBasis = Rand): Rand[SdeParameter] = new Rand[SdeParameter] {

    def draw = {
      val n = self.flatten.length
      val innov = diag(DenseVector.fill(n)(delta).map(1/sqrt(_))) * DenseVector.rand(n, rand.gaussian(0, 1))

      self.add(innov)
    }
  }
  def add(delta: DenseVector[Double]): SdeParameter
  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter
  def mapDbl(f: Double => Double): SdeParameter
}

case class GenBrownianParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
  mu: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Error[SdeParameter] = that match {
    case GenBrownianParameter(m01, c01, m1, s) =>
      Right(SdeParameter.genBrownianParameter(m0 + m01: _*)(c0 + c01: _*)(mu + m1: _*)(sigma + s: _*))
    case _ => Left(throw new Exception(s"Can't sum Brownianparameter with $that"))
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val m = m0 + delta(m0.indices)
    val c = c0 + delta(m0.length to (m0.length + c0.length - 1))
    val m1 = mu + delta((m0.length + c0.length) to (m0.length + c0.length + mu.length - 1))
    val s1 = sigma + delta((m0.length + c0.length + mu.length) to - 1)

    SdeParameter.genBrownianParameter(m: _*)(c: _*)(m1: _*)(s1: _*)
  }

  def flatten: Seq[Double] = m0 ++ c0 ++ mu ++ sigma

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter = {
    SdeParameter.genBrownianParameter(f(m0): _*)(f(c0): _*)(f(mu): _*)(f(sigma): _*)
  }

  def mapDbl(f: Double => Double): SdeParameter = {
    SdeParameter.genBrownianParameter(m0.map(f): _*)(c0.map(f): _*)(mu.map(f): _*)(sigma.map(f): _*)
  }

  def toMap: Map[String, Double] = {
    SdeParameter.denseVectorToMap(m0, "m0") ++ SdeParameter.denseVectorToMap(c0, "c0") ++
    SdeParameter.denseVectorToMap(mu, "mu") ++ SdeParameter.denseVectorToMap(sigma, "sigma")
  }
}

case class BrownianParameter(
  m0: DenseVector[Double], 
  c0: DenseVector[Double], 
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Error[SdeParameter] = that match {
    case BrownianParameter(m01, c01, c1) =>
      Right(SdeParameter.brownianParameter(m0 + m01: _*)(c0 + c01: _*)(sigma + c1: _*))
    case _ => Left(throw new Exception(s"Can't sum Brownianparameter with $that"))
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val m = m0 + delta(m0.indices)
    val c = c0 + delta(m0.length to (m0.length + c0.length - 1))
    val s = sigma + delta((m0.length + c0.length) to (m0.length + c0.length + sigma.length - 1))

    SdeParameter.brownianParameter(m: _*)(c: _*)(s: _*)
  }

  def flatten: Seq[Double] = m0 ++ c0 ++ sigma

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter = {
    SdeParameter.brownianParameter(f(m0): _*)(f(c0): _*)(f(sigma): _*)
  }

  def mapDbl(f: Double => Double): SdeParameter = {
    SdeParameter.brownianParameter(m0.map(f): _*)(c0.map(f): _*)(sigma.map(f): _*)
  }
}

case class OuParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
  alpha: DenseVector[Double],
  sigma: DenseVector[Double],
  theta: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): Error[SdeParameter] = that match {
    case OuParameter(m, c, a, s, t) if t.size == theta.size && alpha.size == a.size =>
      Right(SdeParameter.ouParameter(m0 + m: _*)(c0 + c: _*)(alpha + a: _*)(sigma + s: _*)(theta + t: _*))
    case _ => Left(throw new Exception(s"Can't sum OrnsteinParameter with $that"))
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val m = m0 + delta(m0.indices)
    val c = c0 + delta(m0.length to (m0.length + c0.length - 1))
    val a = alpha + delta((m0.length + c0.length) to (m0.length + c0.length + alpha.length - 1))
    val s = sigma + delta((m0.length + c0.length + alpha.length) to (m0.length + c0.length + alpha.length + sigma.length - 1))
    val t = theta + delta(m0.length + c0.length + alpha.length + sigma.length to -1)

    SdeParameter.ouParameter(m: _*)(c: _*)(a: _*)(s: _*)(t: _*)
  }

  def flatten: Seq[Double] =
    m0 ++ c0 ++ alpha ++ sigma ++ theta

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter = {
    SdeParameter.ouParameter(f(m0): _*)(f(c0): _*)(f(alpha): _*)(f(sigma): _*)(f(theta): _*)
  }

  def mapDbl(f: Double => Double): SdeParameter = {
    SdeParameter.ouParameter(m0.map(f): _*)(c0.map(f): _*)(alpha.map(f): _*)(sigma.map(f): _*)(theta.map(f): _*)
  }
}

object SdeParameter {
  implicit def seqToDenseVector(s: Seq[Double]): DenseVector[Double] = DenseVector(s.toArray)
  implicit def denseVectorToSeq(d: DenseVector[Double]): Seq[Double] = d.data.toSeq

  // smart constructors
  def genBrownianParameter(m0: Double*)(c0: Double*)(mu: Double*)(sigma: Double*): SdeParameter = {

    GenBrownianParameter(m0, c0, mu, sigma)
  }

  def brownianParameter(m0: Double*)(c0: Double*)(sigma: Double*): SdeParameter = {

    BrownianParameter(m0, c0, sigma)
  }

  def ouParameter(m0: Double*)(c0: Double*)(alpha: Double*)(sigma: Double*)(theta: Double*): SdeParameter = {

    OuParameter(m0, c0, alpha, sigma, theta)
  }

  def denseVectorToMap(s: DenseVector[Double], name: String): Map[String, Double] = {
    s.data.zipWithIndex.
      map { case (value, i) => (name + "_" + i -> value) }.
      foldLeft(Map[String, Double]())((acc, a) => acc + a)
  }
}
