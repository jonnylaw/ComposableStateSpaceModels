package com.github.jonnylaw.model

import breeze.linalg._
import breeze.stats.covmat

object Utilities {

  /**
    * Calculate the mean and covariance of a sequence of DenseVectors
    */
  def meanCovSamples(samples: Seq[DenseVector[Double]]) = {
    val n = samples.size
    val m = new DenseMatrix(n, samples.head.size, samples.map(_.data).toArray.transpose.flatten)
    val sampleMean = samples.reduce(_ + _) :* 1.0/n
    val sampleCovariance = covmat.matrixCovariance(m)

    (sampleMean, sampleCovariance)
  }

}
