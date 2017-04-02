package com.github.jonnylaw.model

import akka.NotUsed
import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian, Process, MarkovChain, ContinuousDistr}
import breeze.stats.distributions.Rand._
import breeze.stats.distributions.MarkovChain._
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics._
import akka.stream.scaladsl._
import cats._
import cats.data.{Reader, Kleisli}
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
case class ParamsState(ll: LogLikelihood, params: Parameters, accepted: Int) extends Serializable

/**
  * The state of the metropolis-hastings algorithms
  * @param ll the log-likelihood of the observations given the latent state and the current parameters
  * @param params the current set of parameters
  * @param state the proposed path of the state from the particle filter
  * @param accepted the total number of accepted moves in the metropolis hastings algorithm
  */
case class MetropState(ll: LogLikelihood, params: Parameters, sde: StateSpace, accepted: Int) extends Serializable

trait MetropolisHastings[G[_]] {
  implicit def g: Monad[G]

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
  def logLikelihood: Parameters => G[LogLikelihood]

  /**
    * A single step of the metropolis hastings algorithm to be 
    * used with breeze implementation of Markov Chain.
    * This is an alteration to the implementation in breeze, 
    * here ParamsState holds on to the previous 
    * calculated pseudo marginal log-likelihood value so we 
    * don't need to run the previous particle filter again each iteration
    */
  def mhStep(p: ParamsState): G[ParamsState] = {
    val propParams = proposal(p.params).draw 

    for {
      propll <- logLikelihood(propParams)
      a = propll + logTransition(propParams, p.params) + prior(propParams) - 
        logTransition(p.params, propParams) - p.ll - prior(p.params)
      u = Uniform(0, 1).draw
      next = if (log(u) < a) {
        ParamsState(propll, propParams, p.accepted + 1)
      } else {
        p
      }
    } yield next
  }

  def params: Source[ParamsState, NotUsed]
}

/**
  * Implementation of the particle metropolis algorithm
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
  */
case class ParticleMetropolisSerial(
  logLikelihood: Parameters => Id[LogLikelihood],
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  prior: Parameters => LogLikelihood) extends MetropolisHastings[Id] {

  def g = implicitly[Monad[Id]]

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0

  def params: Source[ParamsState, NotUsed] = {
    val initState = ParamsState(-1e99, initialParams, 0)
    Source.unfold(initState)(state => Some((mhStep(state), state)))
  }
}

/**
  * Implementation of the particle metropolis algorithm
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
  */
case class ParticleMetropolisAsync(
  logLikelihood: Parameters => Future[LogLikelihood],
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  prior: Parameters => LogLikelihood)(implicit val ec: ExecutionContext) extends MetropolisHastings[Future] {

  def g = implicitly[Monad[Future]]

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0

  /**
    * Use the same step for iterations in a stream
    */
  def params: Source[ParamsState, NotUsed] = {
    val initState = ParamsState(-1e99, initialParams, 0)
    Source.unfoldAsync(initState)(state => mhStep(state) map ((s: ParamsState) => Some((s, state))))
  }
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
case class ParticleMetropolisHastingsSerial(
  logLikelihood: Parameters => Id[LogLikelihood],
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  proposal: Parameters => Rand[Parameters],
  initialParams: Parameters,
  prior: Parameters => LogLikelihood) extends MetropolisHastings[Id] {

  def g = implicitly[Monad[Id]]

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)

  /**
    * Use the same step for iterations in a stream
    */
  def params: Source[ParamsState, NotUsed] = {
    val initState = ParamsState(-1e99, initialParams, 0)
    Source.unfold(initState)(state => Some((mhStep(state), state)))
  }
}

/**
  * Implementation of the particle metropolis algorithm
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  */
case class ParticleMetropolisHastingAsync(
  logLikelihood: Parameters => Future[LogLikelihood],
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  prior: Parameters => LogLikelihood)(implicit val ec: ExecutionContext) extends MetropolisHastings[Future] {

  def g = implicitly[Monad[Future]]

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0

  def params: Source[ParamsState, NotUsed] = {
    val initState = ParamsState(-1e99, initialParams, 0)
    Source.unfoldAsync(initState)(state => mhStep(state) map ((s: ParamsState) => Some((s, state))))
  }
}

/**
  * Particle Metropolis hastings which also samples the final value of the state
  */
case class ParticleMetropolisState(
  pf: Parameters => Id[(LogLikelihood, Vector[StateSpace])],
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  prior: Parameters => LogLikelihood) extends MetropolisHastings[Id] {

  def g = implicitly[Monad[Id]]

  override def logLikelihood: Parameters => Id[LogLikelihood] = p => pf(p).map(_._1)

  def mhStepState: MetropState => MetropState = s => {
    val propParams = proposal(s.params).draw
    for {
      state <- pf(propParams)
      a = state._1 + logTransition(propParams, s.params) + prior(propParams) - 
      logTransition(s.params, propParams) - s.ll - prior(s.params)
      u = Uniform(0, 1).draw
      prop = if (log(u) < a) {
        MetropState(state._1, propParams, state._2.last, s.accepted + 1)
      } else {
        s
      }
    } yield prop
  }

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0

  def iters: Source[MetropState, NotUsed] = {
    val init = MetropState(-1e99, initialParams, StateSpace(0.0, Tree.leaf(DenseVector())), 0)
    Source.unfold(init)(s => Some((mhStepState(s), s))).drop(1)
  }

  def params: Source[ParamsState, NotUsed] = {
    val initState = ParamsState(-1e99, initialParams, 0)
    Source.unfold(initState)(state => Some((mhStep(state), state)))
  }
}

case class ParticleMetropolisStateAsync(
  pf: Parameters => Future[(LogLikelihood, Vector[StateSpace])],
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters],
  prior: Parameters => LogLikelihood)(implicit val ec: ExecutionContext) extends MetropolisHastings[Future] {

  def g = implicitly[Monad[Future]]

  override def logLikelihood: Parameters => Future[LogLikelihood] = p => pf(p).map(_._1)

  def mhStepState: MetropState => Future[MetropState] = s => {
    val propParams = proposal(s.params).draw
    for {
      state <- pf(propParams)
      a = state._1 + logTransition(propParams, s.params) + prior(propParams) - 
      logTransition(s.params, propParams) - s.ll - prior(s.params)
      u = Uniform(0, 1).draw
      prop = if (log(u) < a) {
        MetropState(state._1, propParams, state._2.last, s.accepted + 1)
      } else {
        s
      }
    } yield prop
  }

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0

  def iters: Source[MetropState, NotUsed] = {
    val init = MetropState(-1e99, initialParams, StateSpace(0.0, Tree.leaf(DenseVector())), 0)
    Source.unfoldAsync(init)(state => mhStepState(state) map ((s: MetropState) => Some((s, state)))).
      drop(1)
  }

  def params: Source[ParamsState, NotUsed] = {
    val initState = ParamsState(-1e99, initialParams, 0)
    Source.unfoldAsync(initState)(state => mhStep(state) map ((s: ParamsState) => Some((s, state))))
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
    prior: Parameters => LogLikelihood): Reader[BootstrapFilter[Id], Source[MetropState, NotUsed]] = Reader { pf =>

    ParticleMetropolisState(pf.run, initP, proposal, prior).iters
  }
}
