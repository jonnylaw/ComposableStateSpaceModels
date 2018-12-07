package com.github.jonnylaw.model

import breeze.linalg.DenseVector

/**
  * Captures the ability to add a dense vector of doubles to an object
  */
trait Addable[F] {
  def add(fa: F, that: DenseVector[Double]): F
}
