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
    * This should feature in the acceptance ratio... Unless the prior is flat
    */
  val initialParams: Rand[Parameters]

  /**
    * The likelihood function of the model, typically a pseudo-marginal likelihood for 
    * the PMMH algorithm
    */
  def logLikelihood: Parameters => LogLikelihood

  /**
    * A single step of the metropolis hastings algorithm
    */
  def mhStep: MetropState => Rand[MetropState] = p => {
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
      p <- initialParams
    } yield (MetropState(logLikelihood(p), p, 0))
    promote(iterate(init)(x => x flatMap mhStep))
  }

  /**
    * Generate an Akka stream of iterations from the MH algorithm with 
    * log likelihood, total accepted and parameters
    */
  def iters: Rand[Source[MetropState, Any]] = {
    baseIters map (Source(_))
  }

  /**
    * Generate an Akka stream of Parameters
    */
  def params: Rand[Source[Parameters, Any]] = {
    iters map (_.map(_.params))
  }
}

case class ParticleMetropolis(
  mll: Parameters => LogLikelihood,
  initParams: Rand[Parameters],
  perturb: Parameters => Rand[Parameters]) extends MetropolisHastings{

  def logLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
  def proposal: Parameters => Rand[Parameters] = perturb
  val initialParams = initParams
}

object ParticleMetropolis {
  def apply(
    mll: Parameters => LogLikelihood,
    initParams: Parameters,
    perturb: Parameters => Rand[Parameters]) = {

    new ParticleMetropolis(mll, new Rand[Parameters] { def draw = initParams }, perturb)
  }
}

case class ParticleMetropolisHastings(
  mll: Parameters => LogLikelihood,
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  propParams: Parameters => Rand[Parameters],
  initParams: Rand[Parameters]) extends MetropolisHastings {

  def logLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)
  def proposal: Parameters => Rand[Parameters] = propParams
  val initialParams = initParams
}


object ParticleMetropolisHastings {
  def apply(
    mll: Parameters => LogLikelihood,
    transitionProb: (Parameters, Parameters) => LogLikelihood,
    propParams: Parameters => Rand[Parameters],
    initParams: Parameters): ParticleMetropolisHastings = {
    new ParticleMetropolisHastings(mll, transitionProb, propParams, new Rand[Parameters] { def draw = initParams })
  }
}
