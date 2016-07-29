package model

import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian, Process, MarkovChain}
import breeze.stats.distributions.Rand._
import breeze.stats.distributions.MarkovChain._
import breeze.linalg.DenseMatrix
import model.POMP._
import akka.stream.scaladsl.Source
import akka.stream.scaladsl._
import Stream._

case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int) {
  override def toString = s"${params.toString}, $accepted"
}

// TODO: Figure out why streaming MCMC is not working

trait MetropolisHastings {

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

  val initialParams: Parameters

  /**
    * The likelihood function of the model, typically a pseudo-marginal likelihood for 
    * the PMMH algorithm
    */
  def logLikelihood: Parameters => LogLikelihood

  /**
    * Simple metropolis hastings step, drawing from the proposal distribution
    */
  def mhStep: MetropState => MetropState = p => {
    val propParams = proposal(p.params).draw
    val propll = logLikelihood(propParams)
    val a = propll - p.ll + logTransition(propParams, p.params) - logTransition(p.params, propParams)

    if (math.log(Uniform(0,1).draw) < a) {
      MetropState(propll, propParams, p.accepted + 1)
    } else {
      p
    }
  }

  /**
    * Generates an akka stream of MetropState, containing the current parameters, 
    * count of accepted moves and the current pseudo marginal log-likelihood
    * Unfortunately for an unknown reason this isn't working
    */
  def itersAkka: Source[MetropState, Any] = {
    val initState = MetropState(-1e99, initialParams, 0)
    Source.unfold(initState)(s => Some((mhStep(s), s)))
  }

  /**
    * Return an akka stream of the parameters
    */
  def paramsAkka: Source[Parameters, Any] = {
    itersAkka map (_.params)
  }

  /**
    * A single step of the metropolis hastings algorithm to be 
    * used with breeze implementation of Markov Chain.
    * This is a slight alteration to the implementation in breeze, 
    * here MetropState holds on to the previous 
    * calculated pseudo marginal log-likelihood value so we 
    * don't need to run the previous particle filter again each iteration
    */
  def mhStepRand: MetropState => Rand[MetropState] = p => {
    for {
      propParams <- proposal(p.params)
      propll = logLikelihood(propParams)
      a = propll - p.ll + logTransition(propParams, p.params) - logTransition(p.params, propParams)
      prop = if (math.log(Uniform(0,1).draw) < a) {
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
  def iters: Process[MetropState] = {
    val initState = MetropState(-1e99, initialParams, 0)
    MarkovChain(initState)(mhStepRand)
  }

  /**
    * Returns iterations from the MCMC algorithm in a vector
    * using sampleStep, sampleStep 
    */
  def itersVector(n: Int): Vector[MetropState] = {
    MetropolisHastings.sampleStep(n, iters)
  }

  /**
    * Use the same step for iterations in a stream
    */
  def itersStream: Source[MetropState, Any] = {
    val initState = MetropState(-1e99, initialParams, 0)
    Source.unfold(initState)(s => Some((mhStepRand(s).draw, s)))
  }

  def params: Rand[Parameters] = {
    iters map (_.params)
  }
}

object MetropolisHastings {
  def sampleStep[T](n: Int, p: Process[T]): Vector[T] = {
    def go(p: Process[T], acc: Vector[T]): Vector[T] = {
      if (acc.size == n) {
        acc
      } else {
        val draw = p.step
        go(draw._2, acc :+ draw._1)
      }
    }
    go(p, Vector[T]())
  }
}

case class ParticleMetropolis(
  mll: Parameters => LogLikelihood,
  initParams: Parameters,
  perturb: Parameters => Rand[Parameters]) extends MetropolisHastings{

  def logLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
  def proposal: Parameters => Rand[Parameters] = perturb
  val initialParams = initParams
}

case class ParticleMetropolisHastings(
  mll: Parameters => LogLikelihood,
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  propParams: Parameters => Rand[Parameters],
  initParams: Parameters) extends MetropolisHastings {

  def logLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)
  def proposal: Parameters => Rand[Parameters] = propParams
  val initialParams = initParams
}

case class ParticleMetropolisRand(
  mll: Parameters => Rand[LogLikelihood],
  initParams: Parameters,
  perturb: Parameters => Rand[Parameters]) extends MetropolisHastings{

  def logLikelihood: Parameters => LogLikelihood = p => mll(p).draw
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
  def proposal: Parameters => Rand[Parameters] = perturb
  val initialParams = initParams
}
