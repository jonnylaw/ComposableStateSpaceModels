package com.github.jonnylaw.model 

import breeze.numerics._
import ParticleFilter._
import breeze.linalg._
import breeze.stats.distributions._
import breeze.stats._
import akka.stream._
import scaladsl._
import akka.NotUsed
import State._
import Parameters._

sealed trait SufficientStatistics

/**
  * The Gamma distribution is conjugate to the normal distribution with unknown precision (1 / variance)
  */
case class GammaStatistics(dist: Gamma) extends SufficientStatistics

/**
  * State for the Storvik Filter
  */
case class StorvikState(
  t: Time, 
  state: Seq[State], 
  eta: Seq[Eta], 
  p: Seq[Parameters], 
  stats: Seq[SufficientStatistics], 
  ess: Int) {

 override def toString = {
   val state_mean = meanState(state).flatten.mkString(", ")
   val state_intervals = getAllCredibleIntervals(state, 0.975).mkString(", ")
   val mean_params = getMeanParams(p)
//   val param_intervals = getParameterIntervals(p).mkString(", ")

   s"$t, $state_mean, $state_intervals, $mean_params, $ess"
 }
}

// Eventually this will extend the particle filter trait and (initially) 
// two particle filters will be available: Storvik and Bootstrap etc.
case class StorvikGamma(
  mod: UnparamModel, 
  propose: SufficientStatistics => Rand[Parameters], 
  n: Int, 
  hyperParams: Gamma) {

  def init(p: Parameters): StorvikState = {
    val initState = mod(p).x0.sample(n)

    val initEta = for {
      x <- initState
      zeta = mod(p).f(x, 0.0)
      eta = mod(p).link(zeta)
    } yield eta

    StorvikState(0.0, initState, initEta, Seq.fill(n)(p), Seq.fill(n)(GammaStatistics(hyperParams)), 0)
  }

  /**
    * Since the observation is univariate, we know the value of eta is a single value
    * so we take the only value contained inside of the matrix
    */
  def updateStats(s: Gamma, eta: Eta, eta1: Eta): GammaStatistics = {
    val shape = s.shape + 0.5
    val scale = 1.0/s.scale + 0.5 * ((eta1.head - eta.head)*(eta1.head - eta.head))
    GammaStatistics(Gamma(shape, 1.0 / scale))
  }

  def stepFilter(p: Parameters): (StorvikState, Data) => StorvikState = (s, d) => {
    // propose parameters from a known distribution, using the vector of sufficient statistics
    val params = s.stats map ((x: SufficientStatistics) => propose(x).draw)

    // advance the state particles, according to p(x(t) | x(t-1), theta)
    val advancedParticles = s.state.zip(params) map { case (x, p) => mod(p).stepFunction(x, d.t - s.t).draw }

    // transform the particles to the parameter space of the observation distribution
    val eta = for {
      (x, p) <- advancedParticles.zip(params)
      zeta = mod(p).f(x, d.t)
      eta = mod(p).link(zeta)
    } yield eta

    // calculate the new weights, evaluating p(y(t) | x(t), theta)
    val weights = eta.zip(params) map { case (e, p) => mod(p).dataLikelihood(e, d.observation) }

    // log-sum-exp trick
    val maxWeight = weights.max
    val expWeight = weights map (a => exp(a - maxWeight))

    // normalise the weights to sum to one
    val normWeights = normalise(expWeight)

    // calculate the effective sample size, could use this to decide whether to resample
    val ess = effectiveSampleSize(normWeights)

    // we are resampling each time
    val indices = multinomialResampling(s.state.indices, expWeight)
    val resampledState = indices map (advancedParticles(_))
    val resampledEta = indices map (eta(_))
    val resampledParams = indices map (params(_))
    val resampledStats = indices map (s.stats(_))

    val stats = s.eta.zip(resampledEta).zip(resampledStats).
      map { case ((e, e1), stat: GammaStatistics) => updateStats(stat.dist, e, e1) }

    StorvikState(d.t, resampledState, resampledEta, resampledParams, stats, ess)
  }

  def filter(p: Parameters): Flow[Data, StorvikState, NotUsed] = {
    Flow[Data].scan(init(p))(stepFilter(p))
  }
}
