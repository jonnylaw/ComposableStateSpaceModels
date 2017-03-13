package com.github.jonnylaw.model

import breeze.linalg._
import breeze.stats.distributions._
import math.sqrt

/**
  * Simulate from a multivariate normal using Eigenvalue decomposition
  * Y = Q * Z + mean, where Q = L^0.5 * M
  */
case class MultivariateNormal(mean: DenseVector[Double], covariance: DenseMatrix[Double])
  (implicit rand: RandBasis = Rand) extends Rand[DenseVector[Double]] {
  def eigen = eigSym(covariance)

  /** Normalise the matrix of eigenvalues to build the matrix **/
  def q = {
    eigen.eigenvectors * diag(eigen.eigenvalues.mapValues(sqrt))
  }

  def draw = {
    q * DenseVector.rand(mean.length, rand.gaussian(0, 1)) + mean
  }
}
