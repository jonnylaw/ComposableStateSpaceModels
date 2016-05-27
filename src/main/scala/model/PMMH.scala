package model

import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian}
import breeze.linalg.DenseMatrix
import model.POMP._
import akka.stream.scaladsl.Source
import akka.stream.scaladsl._

case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int)

trait MetropolisHastings {

  // define the proposal distribution and transition (log) probability
  def proposal: Parameters => Rand[Parameters]
  def logTransition(from: Parameters, to: Parameters): LogLikelihood
  val initialParams: Parameters

  // represented by a particle filter in PMMH, expensive to compute
  def marginalLogLikelihood: Parameters => LogLikelihood

  def mhStep: MetropState => Option[(MetropState, MetropState)] = p => {
    val propParams = proposal(p.params).draw
    val propll = marginalLogLikelihood(propParams)

    val a = propll - p.ll + logTransition(propParams, p.params) - logTransition(p.params, propParams)

    if (math.log(Uniform(0,1).draw) < a) {
      Some((MetropState(propll, propParams, p.accepted + 1), p))
    } else {
      Some((p, p))
    }
  }

  def iters: Source[MetropState, Any] = {
    val init =  MetropState(marginalLogLikelihood(initialParams), initialParams, 0)
    Source.unfold(init)(mhStep)
  }
}

case class ParticleMetropolis(
  mll: Parameters => LogLikelihood,
  initParams: Parameters,
  perturb: Parameters => Rand[Parameters]) extends MetropolisHastings{

  def marginalLogLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
  def proposal: Parameters => Rand[Parameters] = perturb
  val initialParams = initParams
}

case class ParticleMetropolisHastings(
  mll: Parameters => LogLikelihood,
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  propParams: Parameters => Rand[Parameters],
  initParams: Parameters) extends MetropolisHastings {

  def marginalLogLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)
  def proposal: Parameters => Rand[Parameters] = propParams
  val initialParams = initParams
}
