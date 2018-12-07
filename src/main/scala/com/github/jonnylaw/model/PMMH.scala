package com.github.jonnylaw.model

import akka.NotUsed
import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian, Process, MarkovChain, ContinuousDistr}
import breeze.stats.distributions.Rand._
import breeze.stats.distributions.MarkovChain._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics._
import akka.stream.scaladsl._
import cats._
import cats.data.Reader
import scala.language.higherKinds
import cats.implicits._
import scala.concurrent._

/**
  * The state of the metropolis-hastings algorithms
  * @param ll the log-likelihood of the observations given the latent state and the current parameters
  * @param params the current set of parameters
  * @param state the current path of the state
  * @param accepted the total number of accepted moves in the metropolis hastings algorithm
  */
case class ParamsState[P](ll: LogLikelihood, params: P, accepted: Int)

/**
  * The state of the metropolis-hastings algorithms
  * @param ll the log-likelihood of the observations given the latent state and the current parameters
  * @param params the current set of parameters
  * @param state the proposed path of the state from the particle filter
  * @param accepted the total number of accepted moves in the metropolis hastings algorithm
  */
case class MetropState[P, S](ll: LogLikelihood, params: P, state: S, accepted: Int)

trait MetropolisHastings[P, S] {
  /**
    * Prior distribution for the parameters, with default implementation
    */
  def prior: P => LogLikelihood

  /**
    * Proposal density, to propose new parameters for a model
    */
  def proposal: P => Rand[P]

  /**
    * Definition of the log-transition, used when calculating the acceptance ratio
    * This is the probability of moving between parameters according to the proposal distribution
    * Note: When using a symmetric proposal distribution (eg. Normal) this cancels in the acceptance ratio
    * @param from the previous parameter value
    * @param to the proposed parameter value
    */
  def logTransition(from: P, to: P): LogLikelihood

  /**
    * The initial parameters, representing the place the Metropolis hastings algorithm starts
    */
  val initialParams: P

  /**
    * A particle filter to calculate the pseudo-marginal
    * likelihood of the composable model
    * the bootstrap particle filter for the PMMH algorithm
    */
  def pf: BootstrapFilter[P, S]

  /**
    * A single step of the metropolis hastings algorithm to be
    * used with breeze implementation of Markov Chain.
    * This is an alteration to the implementation in breeze,
    * here ParamsState holds on to the previous
    * calculated pseudo marginal log-likelihood value so we
    * don't need to run the previous particle filter again each iteration
    */
  def mhStep: MetropState[P, S] => Rand[MetropState[P, S]] = s => {
    for {
      propParams <- proposal(s.params)
      state = pf(propParams)
      a = state._1 + logTransition(propParams, s.params) + prior(propParams) - 
      logTransition(s.params, propParams) - s.ll - prior(s.params)
      u <- Uniform(0, 1)
      prop = if (log(u) < a) {
        MetropState[P, S](state._1, propParams, state._2.last, s.accepted + 1)
      } else {
        s
      }
    } yield prop
  }

  val init: MetropState[P, S]

  def markovIters: Process[MetropState[P, S]] = {
    MarkovChain(init)(mhStep)
  }

  def params: Source[ParamsState[P], NotUsed] = {
    Source.fromIterator(() => markovIters.steps).
      drop(1).
      map(s => ParamsState(s.ll, s.params, s.accepted))
  }

  def iters: Source[MetropState[P, S], NotUsed] = {
    Source.fromIterator(() => markovIters.steps).
      drop(1)
  }
}

/**
  * Particle Metropolis algorithm which also samples the final value of the state
  * @param pf a particle filter which estimates the value of the likelihood and
  * samples the path
  * @param initialParams the starting parameters for the metropolis algorithm
  * param proposal the proposal distribition of the parameters
  * @param transitionProp the probability of transitioning from the previous set of
  *  parameters to the newly proposed set of parameters, for symmetric proposal distributions this
  * always evaluates to zero
  * @param proposal a generic proposal distribution for the metropolis algorithm (eg. Gaussian)
  * @param parameterTransition a function2 (from, to) => LogLikelihood of the probability of moving between parameter proposals
  * @param prior a function representing the the prior distriution over the parameters
  */
case class ParticleMetropolisHastings(
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  parameterTransition: (Parameters, Parameters) => LogLikelihood,
  prior: Parameters => LogLikelihood,
  pf: BootstrapFilter[Parameters, StateSpace[State]]) extends MetropolisHastings[Parameters, StateSpace[State]] {

  val init = MetropState[Parameters, StateSpace[State]](-1e99, initialParams, StateSpace[State](0.0, Tree.leaf(DenseVector())), 0)
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = parameterTransition(from, to)
}

/**
  * Recalculate the likelihood from the previous time point
  */
case class ApproxPMMH(
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  parameterTransition: (Parameters, Parameters) => LogLikelihood,
  prior: Parameters => LogLikelihood,
  pf: BootstrapFilter[Parameters, StateSpace[State]]) extends MetropolisHastings[Parameters, StateSpace[State]] {

  val init = MetropState[Parameters, StateSpace[State]](-1e99, initialParams, StateSpace[State](0.0, Tree.leaf(DenseVector())), 0)
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = parameterTransition(from, to)

  override def mhStep: MetropState[Parameters, StateSpace[State]] => Rand[MetropState[Parameters, StateSpace[State]]] = s => {
    for {
      propParams <- proposal(s.params)
      state = pf(propParams)
      oldState = pf(s.params)
      a = state._1 + logTransition(propParams, s.params) + prior(propParams) -
      logTransition(s.params, propParams) - oldState._1 - prior(s.params)
      u <- Uniform(0, 1)
      prop = if (log(u) < a) {
        MetropState(state._1, propParams, state._2.last, s.accepted + 1)
      } else {
        MetropState(oldState._1, s.params, oldState._2.last, s.accepted)
      }
    } yield prop
  }
}


object MetropolisHastings {
  /**
    * Determine the joint posterior distribution p(x, theta | y) of a POMP model
    * @param initP the initial parameters of the PMMH algorithm
    */
  def pmmhState(
    initP: Parameters,
    proposal: Parameters => Rand[Parameters],
    logTransition: (Parameters, Parameters) => LogLikelihood,
    prior: Parameters => LogLikelihood) = Reader { (pf: BootstrapFilter[Parameters, StateSpace[State]]) =>
    ParticleMetropolisHastings(initP, proposal, logTransition, prior, pf).iters
  }

  def approxPmmh(
    initP: Parameters,
    proposal: Parameters => Rand[Parameters],
    logTransition: (Parameters, Parameters) => LogLikelihood,
    prior: Parameters => LogLikelihood) = Reader { (pf: BootstrapFilter[Parameters, StateSpace[State]]) =>
    ApproxPMMH(initP, proposal, logTransition, prior, pf).iters
  }

  def pmmhStep[P](pos: P => LogLikelihood,
                  proposal: P => Rand[P])
              (s: (Double, P)): Rand[(Double, P)] = {
    for {
      prop <- proposal(s._2)
      ll = pos(prop)
      a = ll - s._1
      u <- Uniform(0, 1)
      next = if (log(u) < a) {
        (ll, prop)
      } else {
        s
      }
    } yield next
  }
}

