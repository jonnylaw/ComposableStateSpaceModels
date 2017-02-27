package com.github.jonnylaw.model

import akka.stream.scaladsl._
import akka.stream._
import akka.NotUsed

import breeze.stats.distributions.{Rand, Multinomial}
import breeze.numerics.{exp, log}
import breeze.linalg.DenseVector

import cats._
import cats.data.Reader
import cats.implicits._

import scala.collection.parallel.immutable.ParVector
import scala.collection.immutable.TreeMap
import scala.concurrent._
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import simulacrum._
import Collection.ops._

object Resampling {
  /**
    * Given a vector of doubles, returns a normalised vector with probabilities summing to one
    * @param prob a vector of unnormalised probabilities
    * @return a vector of normalised probabilities
    */
  def normalise[F[_]](prob: F[Double])(implicit f: Collection[F]): F[Double] = {
    val total = ParticleFilter.sum(prob)
    f.map(prob)(x => x/total)
  }

  /**
    * Given a list of ordered doubles, k, find the element at the corresponding 
    * position in the empirical cumulative distribution function represented by a 
    * treeMap
    */
  def findAllInTreeMap[A](ks: Vector[Double], ecdf: TreeMap[Double, A]): Vector[A] = {
    def loop(acc: Vector[A], remMap: TreeMap[Double, A], remK: Vector[Double]): Vector[A] = remK match {
      case Vector() => acc
      case k +: ks => {
        val m = remMap.from(k)
        loop(acc :+ m.head._2, m, ks)
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

    TreeMap.empty[Double, A] ++ (normalisedWeights.scanLeft(0.0)(_ + _).drop(1)).zip(items)
  }

  /**
    * An efficient parallel implementation of of systematic resampling
    */
  def asyncTreeSystematicResampling[A](threads: Int)(
    particles: Vector[A], 
    weights: Vector[LogLikelihood])(implicit ec: ExecutionContext): Future[Vector[A]] = {

    val n = weights.size
    val ecdf = treeEcdf(particles, weights)

    val u = scala.util.Random.nextDouble

    val res = Vector.range(0, n).
      map(i => (u + i) / n).
      grouped(n / threads).
      toVector.
      map((ks: Vector[Double]) => Future { findAllInTreeMap(ks, ecdf) })

    Future.sequence(res).map(_.flatten)
  }

  /**
    * An efficient implementation of of systematic resampling
    */
  def treeSystematicResampling[A](
    particles: Vector[A], weights: Vector[LogLikelihood]): Vector[A] = {

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
  def treeStratifiedResampling[A](s: Vector[A], w: Vector[Double]): Vector[A] = {
    val n = s.size
    val ecdf = treeEcdf(s, w)

    val ks = Vector.range(0, n).map(i => (i + scala.util.Random.nextDouble) / n)

    findAllInTreeMap(ks, ecdf)
  }

 /**
    * Multinomial Resampling, sample from a categorical distribution with probabilities
    * equal to the particle weights 
    */
  def serialMultinomialResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]) = {
    val indices = Vector.fill(particles.size)(Multinomial(DenseVector(weights.toArray)).draw)

    indices map (particles(_))
  }


  def parMultinomialResampling[A](particles: ParVector[A], weights: ParVector[LogLikelihood]) = {
    val indices = ParVector.fill(particles.size)(Multinomial(DenseVector(weights.toArray)).draw)

    indices map (particles(_))
  }

  /**
    * Given a vector of log-likelihoods, normalise them and exp them without overflow
    * @param prob 
    */
  def expNormalise[F[_]](prob: F[LogLikelihood])(implicit f: Collection[F]): F[Double] = {
    val max = f.max(prob)
    val w1 = f.map(prob)(w => exp(w - max))
    val total = ParticleFilter.sum(w1)

    f.map(w1)(w => w / total)
  }

  /**
    * Generic cumulative sum
    */
  def cumSum[F[_], A](l: F[A])(implicit f: Collection[F], N: Numeric[A]): F[A] = {
    f.scanLeft(l, N.zero)((a, b) => N.plus(a, b))
  }

  /**
    * Calculate the empirical cumulative distribution function for a collection of weights
    */
  def empDist[F[_]](w: F[Double])(implicit f: Collection[F]): F[Double] = {
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

    val i = serialMultinomialResampling(Vector.range(1, m), residualWeights)
    x ++ (i map { particles(_) })
  }

  /**
    * Sample one thing, uniformly, from a collection F
    */
  def sampleOne[F[_], A](s: F[A])(implicit f: Collection[F]): A = {
    val index = math.abs(scala.util.Random.nextInt) % f.size(s).toInt
    f.get(s)(index)
  }

  /**
    * Sample unifomly without replacement
    */
  def sampleMany[A](n: Int, s: Vector[A]) = {
    val indices = breeze.linalg.shuffle(s.indices).take(n)
    indices map (i => s(i))
  }
}
