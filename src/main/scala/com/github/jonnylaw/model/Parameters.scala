package com.github.jonnylaw.model

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.stats.covmat
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import breeze.numerics.exp
import cats._
import cats.implicits._
import scala.util.{Try, Success, Failure}

sealed trait Parameters { self =>
  def sum(that: Parameters): Try[Parameters]

  def perturb(delta: Double): Rand[Parameters]

  /**
    * Adds the value delta to the parameters
    */
  def add(delta: DenseVector[Double]): Parameters = self match {
    case BranchParameter(l, r) =>
      Parameters.branchParameter(l.add(delta(0 to l.length - 1)), r.add(delta(l.length to -1)))
    case LeafParameter(scale, sdeParam) => scale match {
      case Some(v) => 
        val sde = sdeParam.add(delta(1 to -1))
        Parameters.leafParameter(Some(v + delta(0)), sde)
      case None =>
        Parameters.leafParameter(None, sdeParam.add(delta))
    }
    case EmptyParameter => Parameters.emptyParameter
  }

  def flatten: Vector[Double]

  def length: Int = this.flatten.length

  def map(f: Double => Double): Parameters

  def toMap: Map[String, Double] = self match {
    case BranchParameter(l, r) => l.toMap ++ r.toMap
    case LeafParameter(s, sde) => s match {
      case Some(v) => Map("scale" -> v) ++ sde.toMap
      case None => sde.toMap
    }
    case EmptyParameter => Map[String, Double]()
  }

  /**
    * Propose a new value of the parameters using a Multivariate Normal distribution
    * Using the cholesky decomposition of the covariance matrix
    * @param scale a scaling factor for the proposal distribution
    * @param sigma the covariance of the proposal distribution
    * @return a distribution over the parameters which can be drawn from
    */
  def perturbMvn(scale: Double, sigma: DenseMatrix[Double]): Rand[Parameters] = {
    MultivariateGaussian(DenseVector.zeros[Double](sigma.cols), scale * scale * sigma) map { innov =>
      self.add(innov)
    }
  }

  /**
    * Propose a new value of the parameters using a Multivariate Normal distribution
    * using the eigen decomposition of the covariance matrix
    * @param scale a scaling factor for the proposal distribution
    * @param sigma the covariance of the proposal distribution
    * @return a distribution over the parameters which can be drawn from
    */
  def perturbMvnEigen(scale: Double, sigma: DenseMatrix[Double]): Rand[Parameters] = {
    MultivariateNormal(DenseVector.zeros[Double](sigma.cols), scale * scale * sigma) map { innov =>
      self.add(innov)
    }
  }
}
case class LeafParameter(scale: Option[Double], sdeParam: SdeParameter) extends Parameters {

  def sum(that: Parameters): Try[Parameters] = that match {
    case LeafParameter(otherScale, sde) =>
      for {
        sdeSum <- sdeParam sum sde
        scaleSum = scale flatMap (v1 => otherScale map (v2 => v1 + v2))
      } yield Parameters.leafParameter(scaleSum, sdeSum)
    case _ => Failure(throw new Exception(s"Can't sum LeafParameter and $that"))
  }

  def flatten: Vector[Double] = scale match {
    case Some(v) => Vector(v) ++ sdeParam.flatten
    case None => sdeParam.flatten
  }

  def perturb(delta: Double): Rand[Parameters] = {
    for {
      sde <- sdeParam.perturb(delta)
      innov <- Gaussian(0.0, delta)
      v = scale map (_ * exp(innov))
    } yield Parameters.leafParameter(v, sde)
  }

  def map(f: Double => Double): Parameters = {
    Parameters.leafParameter(scale.map(f), sdeParam.map(f))
  }
}

case class BranchParameter(left: Parameters, right: Parameters) extends Parameters {
  def sum(that: Parameters): Try[Parameters] = that match {
    case BranchParameter(l, r) => 
      for {
        sumLeft <- left sum l
        sumRight <- right sum r
      } yield Parameters.branchParameter(sumLeft, sumRight)
    case _ => Failure(throw new Exception(s"Can't add BranchParameter and $that"))
  }

  def perturb(delta: Double): Rand[Parameters] = {
    for {
      l <- left.perturb(delta)
      r <- right.perturb(delta)
    } yield Parameters.branchParameter(l, r)
  }

  def flatten: Vector[Double] = left.flatten ++ right.flatten

  def map(f: Double => Double): Parameters = {
    Parameters.branchParameter(left.map(f), right.map(f))
  }
}

case object EmptyParameter extends Parameters {
  def perturb(delta: Double): Rand[Parameters] = Rand.always(Parameters.emptyParameter)
  def sum(that: Parameters): Try[Parameters] = Success(that)
  def flatten = Vector()
  def map(f: Double => Double): Parameters = EmptyParameter
}

object Parameters {
  def leafParameter(scale: Option[Double], sdeParam: SdeParameter): Parameters = {
    LeafParameter(scale, sdeParam)
  }

  def branchParameter(lp: Parameters, rp: Parameters): Parameters = {
    BranchParameter(lp, rp)
  }

  def emptyParameter: Parameters = EmptyParameter

  /**
    * A monoid to compose parameter values
    */
  implicit def composeParameterMonoid = new Monoid[Parameters] {
    def combine(lp: Parameters, rp: Parameters): Parameters = (lp, rp) match {
      case (EmptyParameter, r) => r
      case (l, EmptyParameter) => l
      case _ => Parameters.branchParameter(lp, rp)
    }

    def empty: Parameters = Parameters.emptyParameter
  }

  /**
    * Sum parameter values
    */
  def sumParameters(lp: Parameters, rp: Parameters): Try[Parameters] = lp sum rp

  /**
    * Calculate the mean of the parameter values
    */
  def meanParameters(params: List[Parameters]): Try[Parameters] = {
    val sum = params.foldLeft(Success(Parameters.emptyParameter): Try[Parameters])((a, b) =>
      a.flatMap(Parameters.sumParameters(_, b)))

    sum.map(_.map(_/params.length))
  }

  def proposeIdent: Parameters => Rand[Parameters] = p => new Rand[Parameters] {
    def draw = p
  }

  def perturb(delta: Double): Parameters => Rand[Parameters] = p => {
    p.perturb(delta)
  }

  def perturbMvn(scale: Double, sigma: DenseMatrix[Double]) = { (p: Parameters) =>
    p.perturbMvn(scale, sigma)
  }

  def perturbMvnEigen(scale: Double, sigma: DenseMatrix[Double]) = { (p: Parameters) =>
    p.perturbMvnEigen(scale, sigma)
  }

  /**
    * Calculate the covariance of a sequence of parameters
    */
  def covariance(samples: Seq[Parameters]): DenseMatrix[Double] = {
    val dim = samples.head.flatten.size
    val m = new DenseMatrix(10000, dim, samples.map(_.flatten.toArray).toArray.transpose.flatten)
    covmat.matrixCovariance(m)
  }

}
