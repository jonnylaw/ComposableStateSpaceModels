package com.github.jonnylaw.model

import breeze.linalg.{DenseVector, diag}
import breeze.stats.distributions._
import breeze.numerics.{exp, sqrt}
import math.log
import cats.{Functor, Applicative, Eq, Traverse, Eval}
import cats.implicits._
import scala.language.implicitConversions
import SdeParameter._
import spire.algebra._
import scala.language.higherKinds

sealed trait SdeParameter { self =>
  def flatten: Seq[Double]

  def length: Int = this.flatten.length

  def sum(that: SdeParameter): SdeParameter

  def perturb(delta: Double)(implicit rand: RandBasis = Rand): Rand[SdeParameter] = {
      val n = self.flatten.length
      val innov = diag(DenseVector.fill(n)(delta).map(1/sqrt(_))) * DenseVector.rand(n, rand.gaussian(0, 1))

      Rand.always(self.add(innov))
  }

  def add(delta: DenseVector[Double]): SdeParameter

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter

  def mapDbl(f: Double => Double): SdeParameter

  def names: List[String]

  def traverse[F[_]: Applicative](f: Double => F[Double]): F[SdeParameter] = self match {
    case GenBrownianParameter(m, c, mu, s) =>
      Applicative[F].map4(traverseDenseVector(m)(f), traverseDenseVector(c)(f), 
        traverseDenseVector(mu)(f), traverseDenseVector(s)(f))(GenBrownianParameter(_, _ , _, _))
    case BrownianParameter(m, c, s) =>
      Applicative[F].map3(traverseDenseVector(m)(f), traverseDenseVector(c)(f), 
        traverseDenseVector(s)(f))(BrownianParameter(_, _ , _))
    case OuParameter(m, c, a, s, t) =>
      Applicative[F].map5(traverseDenseVector(m)(f), traverseDenseVector(c)(f), 
        traverseDenseVector(a)(f), traverseDenseVector(s)(f), traverseDenseVector(t)(f))(OuParameter(_, _ , _, _, _))

  }
}

case class GenBrownianParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
  mu: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): SdeParameter = that match {
    case GenBrownianParameter(m01, c01, m1, s) =>
      SdeParameter.genBrownianParameterUnconstrained(m0 + m01: _*)(c0 + c01: _*)(mu + m1: _*)(sigma + s: _*)
    case _ => throw new Exception(s"Can't sum Brownianparameter with $that")
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val m = m0 + delta(m0.indices)
    val c = c0 + delta(m0.length to (m0.length + c0.length - 1))
    val m1 = mu + delta((m0.length + c0.length) to (m0.length + c0.length + mu.length - 1))
    val s1 = sigma + delta((m0.length + c0.length + mu.length) to - 1)

    SdeParameter.genBrownianParameterUnconstrained(m: _*)(c: _*)(m1: _*)(s1: _*)
  }

  def flatten: Seq[Double] = m0 ++ c0 ++ mu ++ sigma

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter = {
    SdeParameter.genBrownianParameterUnconstrained(f(m0): _*)(f(c0): _*)(f(mu): _*)(f(sigma): _*)
  }

  def mapDbl(f: Double => Double): SdeParameter = {
    SdeParameter.genBrownianParameterUnconstrained(m0.map(f): _*)(c0.map(f): _*)(mu.map(f): _*)(sigma.map(f): _*)
  }

  def toMap: Map[String, Double] = {
    SdeParameter.denseVectorToMap(m0, "m0") ++ SdeParameter.denseVectorToMap(c0, "c0") ++
    SdeParameter.denseVectorToMap(mu, "mu") ++ SdeParameter.denseVectorToMap(sigma, "sigma")
  }

  def names =
    (SdeParameter.denseVectorToMap(m0, "m0") ++
      SdeParameter.denseVectorToMap(c0, "C0") ++
      SdeParameter.denseVectorToMap(mu, "mu") ++
      SdeParameter.denseVectorToMap(sigma, "sigma")).keys.toList
}

case class BrownianParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): SdeParameter = that match {
    case BrownianParameter(m01, c01, c1) =>
      SdeParameter.brownianParameterUnconstrained(m0 + m01: _*)(c0 + c01: _*)(sigma + c1: _*)
    case _ => throw new Exception(s"Can't sum Brownianparameter with $that")
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val m = m0 + delta(m0.indices)
    val c = c0 + delta(m0.length to (m0.length + c0.length - 1))
    val s = sigma + delta((m0.length + c0.length) to (m0.length + c0.length + sigma.length - 1))

    SdeParameter.brownianParameterUnconstrained(m: _*)(c: _*)(s: _*)
  }

  def flatten: Seq[Double] = m0 ++ c0 ++ sigma

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter = {
    SdeParameter.brownianParameterUnconstrained(f(m0): _*)(f(c0): _*)(f(sigma): _*)
  }

  def mapDbl(f: Double => Double): SdeParameter = {
    SdeParameter.brownianParameterUnconstrained(m0.map(f): _*)(c0.map(f): _*)(sigma.map(f): _*)
  }

  def names =
    (SdeParameter.denseVectorToMap(m0, "m0") ++
      SdeParameter.denseVectorToMap(c0, "C0") ++
      SdeParameter.denseVectorToMap(sigma, "sigma")).keys.toList
}

case class OuParameter(
  m0: DenseVector[Double],
  c0: DenseVector[Double],
  phi: DenseVector[Double],
  mu: DenseVector[Double],
  sigma: DenseVector[Double]) extends SdeParameter {

  def sum(that: SdeParameter): SdeParameter = that match {
    case OuParameter(m, c, p, me, s) if me.size == mu.size && phi.size == p.size =>
      SdeParameter.ouParameterUnconstrained(m0 + m: _*)(c0 + c: _*)(phi + p: _*)(mu + me: _*)(sigma + s: _*)
    case _ => throw new Exception(s"Can't sum OrnsteinParameter with $that")
  }

  def add(delta: DenseVector[Double]): SdeParameter = {
    val m = m0 + delta(m0.indices)
    val c = c0 + delta(m0.length to (m0.length + c0.length - 1))
    val a = phi + delta((m0.length + c0.length) to (m0.length + c0.length + phi.length - 1))
    val s = mu + delta((m0.length + c0.length + phi.length) to (m0.length + c0.length + phi.length + mu.length - 1))
    val t = sigma + delta(m0.length + c0.length + phi.length + mu.length to -1)

    SdeParameter.ouParameterUnconstrained(m: _*)(c: _*)(a: _*)(s: _*)(t: _*)
  }

  def flatten: Seq[Double] =
    m0 ++ c0 ++ phi ++ mu ++ sigma

  def map(f: DenseVector[Double] => DenseVector[Double]): SdeParameter = {
    SdeParameter.ouParameterUnconstrained(f(m0): _*)(f(c0): _*)(f(phi): _*)(f(mu): _*)(f(sigma): _*)
  }

  def mapDbl(f: Double => Double): SdeParameter = {
    SdeParameter.ouParameterUnconstrained(m0.map(f): _*)(c0.map(f): _*)(phi.map(f): _*)(mu.map(f): _*)(sigma.map(f): _*)
  }

  def names =
    (SdeParameter.denseVectorToMap(m0, "m0") ++
      SdeParameter.denseVectorToMap(c0, "C0") ++
      SdeParameter.denseVectorToMap(phi, "phi") ++
      SdeParameter.denseVectorToMap(mu, "mu") ++
      SdeParameter.denseVectorToMap(sigma, "sigma")).keys.toList

}

object SdeParameter {
  implicit def seqToDenseVector(s: Seq[Double]): DenseVector[Double] = DenseVector(s.toArray)
  implicit def denseVectorToSeq(d: DenseVector[Double]): Seq[Double] = d.data.toSeq

  // smart constructors for unconstrained parameters
  def genBrownianParameterUnconstrained(m0: Double*)(c0: Double*)
                          (mu: Double*)(sigma: Double*): SdeParameter = {
    GenBrownianParameter(m0, c0, mu, sigma)
  }

  def brownianParameterUnconstrained(m0: Double*)(c0: Double*)
                       (sigma: Double*): SdeParameter = {
    BrownianParameter(m0, c0, sigma)
  }

  def ouParameterUnconstrained(m0: Double*)(c0: Double*)(phi: Double*)
                 (mu: Double*)(sigma: Double*): SdeParameter = {
    OuParameter(m0, c0, phi, mu, sigma)
  }

  // smart constructors for constrained parameters, for the user
  def genBrownianParameter(m0: Double*)(c0: Double*)
                          (mu: Double*)(sigma: Double*): SdeParameter = {
    GenBrownianParameter(m0, c0.map(log), mu, sigma.map(log))
  }

  def brownianParameter(m0: Double*)(c0: Double*)
                                    (sigma: Double*): SdeParameter = {
    BrownianParameter(m0, c0.map(log), sigma.map(log))
  }

  def ouParameter(m0: Double*)(c0: Double*)(phi: Double*)
                              (mu: Double*)(sigma: Double*): SdeParameter = {
    OuParameter(m0, c0.map(log), phi.map(logistic), mu, sigma.map(log))
  }

  /**
    * Calculate the log-odds - transform a function in the interval (0, 1) to the real line
    */
  def logit(p: Double) = {
    log(p) - log(1 - p)
  }

  def logistic(x: Double) = {
    1.0 / (1 + exp(-x))
  }

  implicit def addableSde = new Addable[SdeParameter] {
    def add(fa: SdeParameter, that: DenseVector[Double]): SdeParameter = {
      fa add that
    }
  }

  def denseVectorToMap(s: DenseVector[Double], name: String): Map[String, Double] = {
    s.data.zipWithIndex.
      map { case (value, i) => (name + "_" + i -> value) }.
      foldLeft(Map[String, Double]())((acc, a) => acc + a)
  }

  implicit def addSdeParam = new AdditiveSemigroup[SdeParameter] {
    def plus(x: SdeParameter, y: SdeParameter) = x sum y
  }

  implicit def eqSdeParam(implicit ev: Eq[DenseVector[Double]]) = new Eq[SdeParameter] {
    def eqv(x: SdeParameter, y: SdeParameter): Boolean = (x, y) match {
      case (BrownianParameter(m, c, s), BrownianParameter(m1, c1, s1)) => 
        m === m1 && c === c1 && s === s1
      case (GenBrownianParameter(m, c, s, mu), GenBrownianParameter(m1, c1, s1, mu1)) => 
        m === m1 && c === c1 && s === s1 && mu === mu1
      case (OuParameter(m, c, a, s, t), OuParameter(m1, c1, a1, s1, t1)) => 
        m === m1 && c === c1 && s === s1 && t === t1 && a === a1
      case _ => false
    }
  }

    def traverseDenseVector[G[_]: Applicative](fa: DenseVector[Double])(f: Double => G[Double]): G[DenseVector[Double]] =
      fa.data.toVector.traverse(f).map(x => DenseVector(x.toArray))
}
