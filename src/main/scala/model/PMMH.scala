package model

import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian}
import breeze.linalg.DenseMatrix
import model.POMP._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.stream.scaladsl._

case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int)

trait MetropolisHastings {

  // define the proposal distribution and transition (log) probability
  def proposal: Parameters => Rand[Parameters]
  def logTransition(from: Parameters, to: Parameters): LogLikelihood

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

  def iters(initParams: Parameters): Source[MetropState, Any] = {
    Source.unfold(MetropState(marginalLogLikelihood(initParams), initParams, 0))(mhStep)
  }
}

case class ParticleMetropolis(
  mll: Parameters => LogLikelihood,
  delta: Double) extends MetropolisHastings{

  def marginalLogLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
  def proposal: Parameters => Rand[Parameters] = p => p.perturb(delta)
}

case class ParticleMetropolisHastings(
  mll: Parameters => LogLikelihood,
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  propParams: Parameters => Rand[Parameters]) extends MetropolisHastings {

  def marginalLogLikelihood: Parameters => LogLikelihood = mll
  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)
  def proposal: Parameters => Rand[Parameters] = propParams
}
