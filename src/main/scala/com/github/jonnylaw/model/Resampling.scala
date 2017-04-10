package com.github.jonnylaw.model

import breeze.stats.distributions.Multinomial
import breeze.numerics.exp
import breeze.linalg.DenseVector

import cats.implicits._

import scala.collection.immutable.TreeMap
import scala.concurrent._
import scala.language.higherKinds
import scala.collection._
import Collection.ops._

object Resampling {
  /**
    * Given a vector of doubles, returns a normalised vector with probabilities summing to one
    * @param prob a vector of unnormalised probabilities
    * @return a vector of normalised probabilities
    */
  def normalise[F[_]](prob: F[Double])(implicit f: Collection[F]): F[Double] = {
    val total = prob.foldLeft(0.0)(_ + _)
    prob map (x => x/total)
  }

  /**
    * Resample using the identity
    */
  def indentity[A](samples: Vector[A], weights: Vector[Double]) = samples

  /**
    * Given a list of ordered doubles, k, find the element at the corresponding 
    * position in the empirical cumulative distribution function represented by a 
    * treeMap
    */
  def findAllInTreeMap[A](ks: Vector[Double], ecdf: TreeMap[Double, A]): Vector[A] = {
    def loop(acc: Vector[A], remMap: TreeMap[Double, A], remK: Vector[Double]): Vector[A] = {
      if (remK.isEmpty) {
        acc
      } else {
         val m = remMap.from(remK.head)
        loop(acc :+ m.head._2, m, remK.tail)
      }
    }
    loop(Vector(), ecdf, ks)
  }

  /**
    * Create an empirical cumulative distribution function from a set of particles and associated
    * weights, and represent it as a treeMap
    */
  def treeEcdf[A](items: Vector[A], prob: Vector[Double]): TreeMap[Double, A] = {
    val normalisedWeights = normalise(prob)

    val tree = new TreeMap[Double, A]

    tree ++ (normalisedWeights.scanLeft(0.0)(_ + _).drop(1)).zip(items)
  }

  /**
    * An efficient implementation of systematic resampling
    */
  def systematicResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]) = {

    val ecdf = treeEcdf(particles, weights)

    val u = scala.util.Random.nextDouble

    val n = weights.size
    val ks = Vector.range(0, n).map(i => (u + i) / n)

    findAllInTreeMap(ks, ecdf)
  }

  /**
    * Stratified resampling implemented using a TreeMap
    * Sample n ORDERED uniform random numbers (one for each particle) using a linear transformation of a U(0,1) RV
    */
  def stratifiedResampling[A](s: Vector[A], w: Vector[Double]) = {
    val n = s.size
    val ecdf = treeEcdf(s, w)

    val ks = Vector.range(0, n).
      map(i => (i + scala.util.Random.nextDouble) / n)

    findAllInTreeMap(ks, ecdf)
  }

 /**
    * Multinomial Resampling, sample from a categorical distribution with probabilities
    * equal to the particle weights 
    */
  def multinomialResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]): Vector[A] = {
    val indices = Vector.fill(particles.size)(Multinomial(DenseVector(weights.toArray)).draw)

    indices map (particles(_))
  }

  /**
    * Given a vector of log-likelihoods, normalise them and exp them without overflow
    * @param prob 
    */
  def expNormalise(prob: Vector[LogLikelihood]): Vector[Double] = {
    val max = prob.max
    val w1 = prob map (w => exp(w - max))
    val total = w1.sum

    w1 map (w => w / total)
  }

  /**
    * Generic cumulative sum
    */
  def cumSum[A](l: Vector[A])(implicit N: Numeric[A]): Vector[A] = {
    l.scanLeft(N.zero)((a, b) => N.plus(a, b))
  }

  /**
    * Calculate the empirical cumulative distribution function for a collection of weights
    */
  def empDist(w: Vector[Double]): Vector[Double] = {
    cumSum(normalise(w))
  }

  /**
    * Residual Resampling
    * Select particles in proportion to their weights, ie particle (xi, wi) appears ki = n * wi times
    * Resample m = n - total allocated particles particles according to w = n * wi - ki,
    *  using other resampling technique
    */
  def residualResampling[A](particles: Vector[A], weights: Vector[Double]): Vector[A] = {
    val n = weights.length
    val normalisedWeights = expNormalise(weights)
    val ki = normalisedWeights.
      map (w => math.floor(w * n).toInt)

    val indices = ki.zipWithIndex.
      map { case (n, i) => Vector.fill(n)(i) }.
      flatten
    val x = indices map { particles(_) }
    val m = n - indices.length
    val residualWeights = normalisedWeights.zip(ki) map { case (w, k) => n * w - k }


    val i = multinomialResampling(Vector.range(1, m), residualWeights)
    x ++ (i map { particles(_) })
  }

  /**
    * Sample one thing, uniformly, from a collection F
    */
  def sampleOne[A](s: Seq[A]): A = {
    val index = math.abs(scala.util.Random.nextInt) % s.size
    s(index)
  }

  /**
    * Sample unifomly without replacement
    */
  def sampleMany[A](n: Int, s: Vector[A]) = {
    val indices = breeze.linalg.shuffle(s.indices).take(n)
    indices map (i => s(i))
  }
}
