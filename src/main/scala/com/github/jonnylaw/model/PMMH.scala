// package com.github.jonnylaw.model

// import breeze.stats.distributions.{Uniform, Rand, MultivariateGaussian, Process, MarkovChain, ContinuousDistr}
// import breeze.stats.distributions.Rand._
// import breeze.stats.distributions.MarkovChain._
// import breeze.linalg.DenseMatrix
// import breeze.numerics._
// import akka.stream.scaladsl.Source
// import akka.stream.scaladsl._
// import Stream._
// import scala.util.Try

// /**
//   * The state of the metropolis-hastings algorithms
//   * @param ll the log-likelihood of the observations given the latent state and the current parameters
//   * @param params the current set of parameters
//   * @param accepted the total number of accepted moves in the metropolis hastings algorithm
//   */
// case class MetropState(ll: LogLikelihood, params: Parameters, accepted: Int) {
//   override def toString = s"${params.toString}, $accepted"
// }

// trait MetropolisHastings {

//   /**
//     * Prior distribution for the parameters, with default implementation
//     */
//   def prior: ContinuousDistr[Parameters] = new ContinuousDistr[Parameters] {
//     val draw: Parameters = ???
//     def unnormalizedLogPdf(p: Parameters): Double = 0.0
//     def logNormalizer: Double = 0.0
//   }

//   /**
//     * Proposal density, to propose new parameters for a model
//     */
//   def proposal: Parameters => Rand[Parameters]

//   /**
//     * Definition of the log-transition, used when calculating the acceptance ratio
//     * This is the probability of moving between parameters according to the proposal distribution
//     * Note: When using a symmetric proposal distribution (eg. Normal) this cancels in the acceptance ratio
//     * @param from the previous parameter value
//     * @param to the proposed parameter value
//     */
//   def logTransition(from: Parameters, to: Parameters): LogLikelihood

//   /**
//     * The initial parameters, representing the place the Metropolis hastings algorithm starts
//     */
//   val initialParams: Parameters

//   /**
//     * The likelihood function of the model, typically a pseudo-marginal likelihood estimated using 
//     * the bootstrap particle filter for the PMMH algorithm
//     */
//   def logLikelihood: Parameters => Try[LogLikelihood]

//   /**
//     * A single step of the metropolis hastings algorithm to be 
//     * used with breeze implementation of Markov Chain.
//     * This is a slight alteration to the implementation in breeze, 
//     * here MetropState holds on to the previous 
//     * calculated pseudo marginal log-likelihood value so we 
//     * don't need to run the previous particle filter again each iteration
//     */
//   def mhStep: MetropState => Rand[MetropState] = p => {
//     for {
//       propParams <- proposal(p.params)
//       propll = logLikelihood(propParams)
//       a = propll + logTransition(propParams, p.params) + prior.logPdf(propParams) - logTransition(p.params, propParams) - p.ll - prior.logPdf(p.params)
//       u <- Uniform(0, 1)
//       prop = if (log(u) < a) {
//         MetropState(propll, propParams, p.accepted + 1)
//       } else {
//         p
//       }
//     } yield prop
//   }

//   /**
//     * Use the Breeze Markov Chain to generate a process of MetropState
//     * Calling .sample(n) on this will create a single site metropolis hastings, 
//     * proposing parameters only from the initial supplied parameter values
//     */
//   def markovIters: Process[MetropState] = {
//     val initState = MetropState(-1e99, initialParams, 0)
//     MarkovChain(initState)(mhStep)
//   }

//   /**
//     * Use the same step for iterations in a stream
//     */
//   def iters: Source[MetropState, Any] = {
//     Source.fromIterator(() => markovIters.steps)
//   }
// }

// /**
//   * Implementation of the particle metropolis algorithm without a properly specified prior distribution
//   * @param logLikelihood a function from parameters to LogLikelihood
//   * @param initialParams the starting parameters for the metropolis algorithm
//   * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
//   */
// case class ParticleMetropolis(
//   logLikelihood: Parameters => Try[LogLikelihood],
//   initialParams: Parameters,
//   proposal: Parameters => Rand[Parameters]) extends MetropolisHastings{

//   def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
// }

// /**
//   * Implementation of the particle metropolis hastings algorithm without a properly
//   * specified prior distribution
//   * @param logLikelihood a function from parameters to LogLikelihood
//   * @param initialParams the starting parameters for the metropolis algorithm
//   * @param transitionProp the probability of transitioning from the previous set of
//   *  parameters to the newly proposed set of parameters
//   * @param proposal a generic proposal distribution for the metropolis algorithm (eg. Gaussian)
//   */
// case class ParticleMetropolisHastings(
//   logLikelihood: Parameters => Try[LogLikelihood],
//   transitionProb: (Parameters, Parameters) => LogLikelihood,
//   proposal: Parameters => Rand[Parameters],
//   initialParams: Parameters) extends MetropolisHastings {

//   def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)

// }

// /**
//   * Implementation of the particle metropolis algorithm
//   * @param logLikelihood a function from parameters to LogLikelihood
//   * @param initialParams the starting parameters for the metropolis algorithm
//   * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
//   * @param prior a prior distribution on the parameters
//   */
// case class ParticleMetropolisWithPrior(
//   logLikelihood: Parameters => Try[LogLikelihood],
//   proposal: Parameters => Rand[Parameters],
//   initialParams: Parameters, override val prior: ContinuousDistr[Parameters]) extends MetropolisHastings {

//   def logTransition(from: Parameters, to: Parameters): LogLikelihood = 0.0
// }

// /**
//   * Implementation of the particle metropolis-hastings algorithm
//   * @param logLikelihood a function from parameters to LogLikelihood
//   * @param initialParams the starting parameters for the metropolis algorithm
//   * @param proposal a SYMMETRIC proposal distribution for the metropolis algorithm (eg. Gaussian)
//   * @param prior a prior distribution on the parameters
//   */
// case class ParticleMetropolisHastingsWithPrior(
//   logLikelihood: Parameters => Try[LogLikelihood],
//   transitionProb: (Parameters, Parameters) => LogLikelihood,
//   proposal: Parameters => Rand[Parameters],
//   initialParams: Parameters, override val prior: ContinuousDistr[Parameters]) extends MetropolisHastings {

//   def logTransition(from: Parameters, to: Parameters): LogLikelihood = transitionProb(from, to)
// }
