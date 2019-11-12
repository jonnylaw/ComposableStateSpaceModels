
package com.github.jonnylaw.model

import breeze.stats.distributions.{Rand, Process}

object MarkovChain {
  def apply[T](init: T)(resample: T => Rand[T]): Process[T] = new Process[T] {
    val inner = resample(init);
    def draw() = {
      val next = inner.draw();
      next
    }

    override def observe(x: T) = {
      MarkovChain(x)(resample)
    }
  }
}
