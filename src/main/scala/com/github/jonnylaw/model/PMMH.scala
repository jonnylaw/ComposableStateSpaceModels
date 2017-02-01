package com.github.jonnylaw.model

import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian, Process, MarkovChain, ContinuousDistr}
import breeze.stats.distributions.Rand._
import breeze.stats.distributions.MarkovChain._
import breeze.linalg.DenseMatrix
import breeze.numerics._
import fs2._
import fs2.util.Suspendable

/**
  * The state of the metropolis-hastings algorithms
  * @param ll the log-likelihood of the observations given the latent state and the current parameters
  * @param params the current set of parameters
  * @param accepted the total number of accepted moves in the metropolis hastings algorithm
  */
case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int)

trait MetropolisHastings {

  /**
    * Prior distribution for the parameters, with default implementation
    */
  def prior: Parameters => LogLikelihood

  /**
    * Proposal density, to propose new parameters for a model
    */
  def proposal: Parameters => Rand[Parameters]

  /**
    * Definition of the log-transition, used when calculating the acceptance ratio
    * This is the probability of moving between parameters according to the proposal distribution
    * Note: When using a symmetric proposal distribution (eg. Normal) this cancels in the acceptance ratio
    * @param from the previous parameter value
    * @param to the proposed parameter value
    */
  def logTransition(from: Parameters, to: Parameters): LogLikelihood

  /**
    * The initial parameters, representing the place the Metropolis hastings algorithm starts
    */
  val initialParams: Parameters

  /**
    * The likelihood function of the model, typically a pseudo-marginal likelihood estimated using 
    * the bootstrap particle filter for the PMMH algorithm
    */
  def logLikelihood: Parameters => LogLikelihood

  /**
    * A single step of the metropolis hastings algorithm to be 
    * used with breeze implementation of Markov Chain.
    * This is a slight alteration to the implementation in breeze, 
    * here MetropState holds on to the previous 
    * calculated pseudo marginal log-likelihood value so we 
    * don't need to run the previous particle filter again each iteration
    */
  def mhStep: MetropState => Rand[MetropState] = p => {
    for {
      propParams <- proposal(p.params)
      propll = logLikelihood(propParams)
      a = propll + logTransition(propParams, p.params) + prior(propParams) - logTransition(p.params, propParams) - p.ll - prior(p.params)
      u <- Uniform(0, 1)
      prop = if (log(u) < a) {
        MetropState(propll, propParams, p.accepted + 1)
      } else {
        p
      }
    } yield prop
  }

  /**
    * Use the Breeze Markov Chain to generate a process of MetropState
    * Calling .sample(n) on this will create a single site metropolis hastings, 
    * proposing parameters only from the initial supplied parameter values
    */
  def markovIters: Process[MetropState] = {
    val initState = MetropState(-1e99, initialParams, 0)
    MarkovChain(initState)(mhStep)
  }

  def fromProcess[F[_]: Suspendable, A](iter: Process[A]): Stream[F, A] = {
    Stream.unfold(iter.step){ case (a, p) => Some((a, p.step)) }
  }

  /**
    * Use the same step for iterations in a stream
    */
  def iters[F[_]: Suspendable]: Stream[F, MetropState] = {
    fromProcess[F, MetropState](markovIters)
  }
}

/**
  * Implementation of the particle metropolis algorithm
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
  */
case class ParticleMetropolis(
  logLikelihood: Parameters => LogLikelihood,
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  prior: Parameters => LogLikelihood) extends MetropolisHastings {

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
}

/**
  * Implementation of the particle metropolis hastings algorithm
  * specified prior distribution
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param transitionProp the probability of transitioning from the previous set of
  *  parameters to the newly proposed set of parameters
  * @param proposal a generic proposal distribution for the metropolis algorithm (eg. Gaussian)
  */
case class ParticleMetropolisHastings(
  logLikelihood: Parameters => LogLikelihood,
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  proposal: Parameters => Rand[Parameters],
  initialParams: Parameters,
  prior: Parameters => LogLikelihood) extends MetropolisHastings {

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)

}
