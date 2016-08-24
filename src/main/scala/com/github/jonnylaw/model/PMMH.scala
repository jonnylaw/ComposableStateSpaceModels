package com.github.jonnylaw.model

import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian, Process, MarkovChain, ContinuousDistr}
import breeze.stats.distributions.Rand._
import breeze.stats.distributions.MarkovChain._
import breeze.linalg.DenseMatrix
import com.github.jonnylaw.model.POMP._
import akka.stream.scaladsl.Source
import akka.stream.scaladsl._
import Stream._

/**
  * The state of the metropolis-hastings algorithms
  * @param ll the log-likelihood of the observations given the latent state and the current parameters
  * @param params the current set of parameters
  * @param accepted the total number of accepted moves in the metropolis hastings algorithm
  */
case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int) {
  override def toString = s"${params.toString}, $accepted"
}

trait MetropolisHastings {

  /**
    * Prior distribution for the parameters, with default implementation
    */
  def prior: ContinuousDistr[Parameters] = new ContinuousDistr[Parameters] {
    val draw: Parameters = EmptyParameter
    def unnormalizedLogPdf(p: Parameters): Double = 0.0
    def logNormalizer: Double = 0.0
  }

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
    * Generic metropolis-hastings step, which can be used with the usual acceptance ratio
    * or simplified to the metropolis ratio by specifying the log-transition of the parameters 
    * to be zero
    */
  def mhStep: MetropState => MetropState = p => {
    val propParams = proposal(p.params).draw
    val propll = logLikelihood(propParams)
    val a = prior.logPdf(propParams) + propll + logTransition(propParams, p.params) - p.ll - logTransition(p.params, propParams) - prior.logPdf(propParams)

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
  def itersSeq(n: Int): Seq[MetropState] = {
    MetropolisHastings.sampleStep(n, iters)
  }

  /**
    * Use the same step for iterations in a stream
    */
  def itersStream: Source[MetropState, Any] = {
    val initState = MetropState(-1e99, initialParams, 0)
    Source.unfold(initState)(s => Some((mhStepRand(s).draw, s)))
  }
}

object MetropolisHastings {
  /**
    * This steps a Process object, when the method step is called on a Process object
    * another Process is returned as well as a realisation from the process. Then we must use
    * the newly returned Process object to call step on again in order to draw a sequence of random
    * numbers
    * @param n the number of random numbers to draw from the process
    * @param its a process object, returned by a particle metropolis-hastings class when iters 
    * method is called
    * @return a sequence of realisations from the Process object
    */
  def sampleStep[T](n: Int, its: Process[T]): Seq[T] = {
    (1 to n).scanLeft(its.step)((d, _) => {
        val draw = d._2.step
        draw
      }).map(_._1)
  }
}

/**
  * Implementation of the particle metropolis algorithm without a properly specified prior distribution
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
  */
case class ParticleMetropolis(
  logLikelihood: Parameters => LogLikelihood,
  initialParams: Parameters,
  proposal: Parameters => Rand[Parameters]) extends MetropolisHastings{

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
}

/**
  * Implementation of the particle metropolis hastings algorithm without a properly
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
  initialParams: Parameters) extends MetropolisHastings {

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)

}

/**
  * Implementation of the particle metropolis algorithm
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
  * @param prior a prior distribution on the parameters
  */
case class ParticleMetropolisWithPrior(
  logLikelihood: Parameters => LogLikelihood,
  proposal: Parameters => Rand[Parameters],
  initialParams: Parameters, override val prior: ContinuousDistr[Parameters]) extends MetropolisHastings {

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
}

/**
  * Implementation of the particle metropolis-hastings algorithm
  * @param logLikelihood a function from parameters to LogLikelihood
  * @param initialParams the starting parameters for the metropolis algorithm
  * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
  * @param prior a prior distribution on the parameters
  */
case class ParticleMetropolisHastingsWithPrior(
  logLikelihood: Parameters => LogLikelihood,
  transitionProb: (Parameters, Parameters) => LogLikelihood,
  proposal: Parameters => Rand[Parameters],
  initialParams: Parameters, override val prior: ContinuousDistr[Parameters]) extends MetropolisHastings {

  def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)
}
