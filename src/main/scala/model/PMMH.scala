package model

import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian}
import breeze.stats.distributions.Rand._
import breeze.linalg.DenseMatrix
import model.POMP._
import akka.stream.scaladsl.Source
import akka.stream.scaladsl._
import Stream._

case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int)

trait MetropolisHastings {

  /**
    * 
    */
  def proposal: Parameters => Rand[Parameters]

  /**
    * Definition of the log-transition, used when calculating the acceptance ratio
    */
  def logTransition(from: Parameters, to: Parameters): LogLikelihood


  /**
    * Initial parameters as a distribution to be drawn from
    */
  val initialParams: Parameters

  val initialRandParams: Rand[Parameters] = new Rand[Parameters] { def draw = initialParams }

  /**
    * The likelihood function of the model, typically a pseudo-marginal likelihood for 
    * the PMMH algorithm
    */
  def logLikelihood: Parameters => LogLikelihood

  def mhStep: MetropState => Option[(MetropState, MetropState)] = p => {
    val propParams = proposal(p.params).draw
    val propll = logLikelihood(propParams)
    val a = propll - p.ll + logTransition(propParams, p.params) - logTransition(p.params, propParams)

    if (math.log(Uniform(0, 1).draw)) {
      Some((MetropState(propll, propParams, p.accepted + 1), p))
    } else {
      Some((p, p))
    }
  }

  def iters: Source[MetropState, Any] = {
    val initState = MetropState(logLikelihood(initialParams), initialParams, 0)
    Source.unfold(initState)(mhStep)
  }

  /**
    * A single step of the metropolis hastings algorithm
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
    * Generate a stream of iterations from the MH algorithm
    */
  def baseIters: Rand[Stream[MetropState]] = {
    val init: Rand[MetropState] = for {
      p <- initialRandParams
    } yield (MetropState(logLikelihood(p), p, 0))
    promote(iterate(init)(x => x flatMap mhStepRand))
  }

  /**
    * Generate an Akka stream of iterations from the MH algorithm with 
    * log likelihood, total accepted and parameters
    */
  def randIters: Rand[Source[MetropState, Any]] = {
    baseIters map (Source(_))
  }

  /**
    * Generate an Akka stream of Parameters
    */
  def params: Rand[Source[Parameters, Any]] = {
    randIters map (_.map(_.params))
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
