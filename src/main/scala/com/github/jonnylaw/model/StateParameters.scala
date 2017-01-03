package com.github.jonnylaw.model

import breeze.linalg.{DenseVector, DenseMatrix, diag}
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import breeze.numerics.exp
import cats._
import cats.implicits._
import scala.util.{Try, Success, Failure}

sealed trait StateParameter {

  def sum(that: StateParameter): Try[StateParameter]

  def perturb(delta: Double): Rand[StateParameter]

  def perturbIndep(delta: Array[Double]): Rand[StateParameter]
}

case class GaussianParameter(
  m0: DenseVector[Double],
  c0: DenseMatrix[Double]) extends StateParameter {

  def sum(that: StateParameter): Try[StateParameter] = that match {
    case GaussianParameter(m1, c1) => 
      Success(StateParameter.gaussianParameter(m0 + m1, c0 + c1))
    case _ =>
      Failure(throw new Exception(s"Can't add GaussianParameter to $that"))
  }

  def perturb(delta: Double): Rand[StateParameter] = {
    val d = diag(DenseVector.fill(m0.length)(delta))
    for {
      m <- MultivariateGaussian(m0, d)
      innov <- MultivariateGaussian(DenseVector.zeros(m0.length), d)
      c = diag(diag(c0) :* exp(innov))
    } yield StateParameter.gaussianParameter(m, c)
  }

  def perturbIndep(delta: Array[Double]): Rand[StateParameter] = ???
}

object GaussianParameter {
  def apply(m0: Double, c0: Double): GaussianParameter = {
    GaussianParameter(DenseVector(m0), DenseMatrix(c0))
  }
}

object StateParameter {
  /**
    * Smart constructor for GaussianParameter to help with type inference
    * @param m0 the mean of a Gaussian distribution
    * @param c0 the covariance matrix of a Gaussian distribution
    * @return an initial state parameter; a StateParameter object
    */
  def gaussianParameter(
    m0: DenseVector[Double],
    c0: DenseMatrix[Double]): StateParameter = {

    GaussianParameter(m0, c0)
  }
}
