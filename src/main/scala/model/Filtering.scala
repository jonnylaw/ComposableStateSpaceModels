// package model

// import breeze.numerics.{exp, log}
// import breeze.stats.distributions.{Gaussian, Uniform, Exponential, Rand, ContinuousDistr, Multinomial, MultivariateGaussian}
// import breeze.stats.distributions.Rand._
// import breeze.linalg.{linspace, DenseVector, DenseMatrix, diag}
// import breeze.stats.{mean, variance}
// import scala.collection.parallel.immutable.ParVector
// import scala.concurrent.ExecutionContext.Implicits.global
// import model.POMP._
// import model.Utilities._
// import model.DataTypes._
// import model.State._
// import model.SimData._

// import akka.stream.scaladsl.Source
// import scala.concurrent.Future
// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.stream.scaladsl.Source
// import java.io.File
// import akka.stream.scaladsl._
// import akka.util.ByteString

// object Filtering {
//   /**
//     * Return a vector of lag 1 time differences
//     * @param x a list of times
//     * @return a list of differenced times
//     */
//   def diff(x: Iterable[Time]): Iterable[TimeIncrement] = {
//     (x.tail zip x) map { a => a._1 - a._2 }
//   }

//   /**
//     * Sample integers from 1 to n with replacement according to their associated probabilities
//     * @param n a number matching the number of probabilities
//     * @param prob a vector of probabilities corresponding to the probability of sampling that integer
//     * @return a vector containing the samples
//     */
//   def sample(
//     n: Int,
//     prob: DenseVector[Double]): Vector[Int] = {
//     Multinomial(prob).sample(n).toVector
//   }

// /**
//     * Representation of the state of the particle filter, at each step the previous observation time, t0, and 
//     * particle cloud, particles, is required to compute forward.
//     * The meanState and intervals are recorded in each step, so they can be outputted immediately without having
//     * to calculate these from the particle cloud after
//     */
//   case class PfState(
//     t: Time,
//     observation: Option[Observation],
//     particles: Vector[State],
//     meanEta: Double,
//     intervalEta: CredibleInterval,
//     meanState: State,
//     stateIntervals: IndexedSeq[CredibleInterval],
//     ll: LogLikelihood) {

//     override def toString = observation match {
//       case Some(y) => s"$t, $y, $meanEta, $intervalEta, ${meanState.flatten.mkString(", ")}, ${stateIntervals.mkString(", ")}"
//       case None => s"$t, $meanEta, $intervalEta, ${meanState.flatten.mkString(", ")}, ${stateIntervals.mkString(", ")}"
//     }
//   }

//   /**
//     * Perform one step of a particle filter, for use in scan, this only holds on to the current value of PfState
//     * @param y a single timestamped observation
//     * @param s the state of the particle filter at the time of the last observation
//     * @param mod the parameterised model to be fit to the process
//     * @param n the number of particles used in the filter, increasing means more accuracy but longer computational time
//     * @return the state of the particle filter after observation y
//     */
//   def filterStepScan(y: Data, s: PfState, mod: Model, n: Int): PfState = {
//     val dt = y.t - s.t // calculate time between observations

//     val advancedState = s.particles map (x => if (dt == 0) x else mod.stepFunction(x, dt).draw) // advance particle cloud

//     val transState = advancedState map (a => mod.link(mod.f(a, y.t)))

//     val w1 = transState map (a => mod.dataLikelihood(a, y.observation)) // calculate particle likelihoods

//     val max = w1.max // log sum exp
//     val w = w1 map { a => exp(a - max) }

//     val xInd = sample(n, DenseVector(w.toArray)) // Multinomial resampling
//     val resampledState = xInd map ( advancedState(_) )

//     val meanState = weightedMean(resampledState, w)
//     val intervals = getAllCredibleIntervals(resampledState, 0.995)
//     val eta = mod.link(mod.f(meanState, y.t))
//     val etaIntervals = getOrderStatistic((resampledState map (a => mod.link(mod.f(a, y.t)))).map(_.head), 0.995)
   
//     PfState(y.t, Some(y.observation), resampledState, eta.head, etaIntervals, meanState, intervals, s.ll + max + math.log(breeze.stats.mean(w)))
//   }

//   /**
//     * Perform one step of a particle filter, for use in fold
//     * @param y a single observation
//     * @param ds state for the particle filter
//     * @param mod a POMP model
//     * @param n the number of particles
//     * @return updated state for the particle filter
//     */
//   def filterStep(y: Data, ds: Vector[PfState], mod: Model): Vector[PfState] = {
//     val d = ds.head
//     val n = d.particles.length

//     val dt = y.t - d.t // calculate time between observations

//     val advancedState = d.particles map (x => if (dt == 0) x else mod.stepFunction(x, dt).draw) // advance particle cloud

//     val transState = advancedState map (a => mod.link(mod.f(a, y.t)))

//     val w1 = transState map (a => mod.dataLikelihood(a, y.observation)) // calculate particle likelihoods

//     val max = w1.max // log sum exp
//     val w = w1 map { a => exp(a - max) }

//     val xInd = sample(n, DenseVector(w.toArray)) // Multinomial resampling
//     val resampledState = xInd map ( advancedState(_) )

//     val meanState = weightedMean(resampledState, w)
//     val intervals = getAllCredibleIntervals(resampledState, 0.995)
//     val eta = mod.link(mod.f(meanState, y.t))
//     val etaIntervals = getOrderStatistic((resampledState map (a => mod.link(mod.f(a, y.t)))).map(_.head), 0.995)
   
//     PfState(y.t, Some(y.observation), resampledState, eta.head, etaIntervals, meanState, intervals, ds.head.ll + max + math.log(breeze.stats.mean(w))) +: ds
//   }

//   /**
//     * Particle filter to estimate the filtering distribution, 
//     * returns a function closure from parameters to a vector of state
//     * @param n the number of particles
//     * @param data a vector of data
//     * @param unparamMod a POMP model
//     */
//   def pf(data: Vector[Data],
//     unparamMod: Parameters => Model)(n: Int): Parameters => Vector[PfState] = { p =>

//     // parameterise the model and initialise the state 
//     val mod = unparamMod(p)
//     val particleCloud = Vector.fill(n)(mod.x0.draw)
//     val t0 = data.map(_.t).sorted.head
//     val initState = Vector(PfState(t0, None, particleCloud, 0.0,
//       CredibleInterval(0.0, 0.0), State.zero, IndexedSeq[CredibleInterval](), 0.0))

//     // Run the particle filter
//     // drop the initial state from the final output
//     data.foldLeft(initState)((a, y) => filterStep(y, a, mod)).reverse.drop(1)
//   }

//   /**
//     * Describes the state of the particle filter for the pseudo-marginal likelihood
//     */
//   case class MllState(ll: LogLikelihood, t: Time, particles: Vector[State])

//   /**
//     * One step of a particle filter to calculate the pseudo-marginal likelihood
//     * @param y a single observation
//     * @param mod a POMP model
//     * @return a function from MllState => MllState containing the latest distribution of particles and updated marginal likelihood
//     */
//   def mllStep(y: Data, mod: Model): MllState => MllState = s => {
//     val dt = y.t - s.t // time between observations
//     val n = s.particles.length // the number of particles

//     val advancedState = s.particles map (x => if (dt == 0) x else mod.stepFunction(x, dt).draw) // advance particle cloud

//     val transState = advancedState map (a => mod.link(mod.f(a, y.t)))

//     val w1 = transState map (a => mod.dataLikelihood(a, y.observation)) // calculate particle likelihoods

//     val max = w1.max // log sum exp
//     val w = w1 map { a => exp(a - max) }

//     val xInd = sample(n, DenseVector(w.toArray)) // Multinomial resampling
//     val resampledState = xInd map ( advancedState(_) )

//     MllState(s.ll + max + log(breeze.stats.mean(w)), y.t, resampledState)
//   }

//   /**
//     * A function to calculate the pseudo-marginal likelihood using mllStep
//     * @param data a vector of observations
//     * @param unparamMod an unparameterised model
//     * @param n the number of particles to use in the filter
//     * @return a function from parameters to LogLikelihood
//     */
//   def pfMllFold(data: Vector[Data],
//     unparamMod: Parameters => Model)(n: Int): Parameters => LogLikelihood = p => {

//     val mod = unparamMod(p)
//     val particleCloud = Vector.fill(n)(mod.x0.draw)
//     val t0 = data.map(_.t).sorted.head
//     val initState = MllState(0.0, t0, particleCloud)

//     data.foldLeft(initState)((s, y) => mllStep(y, mod)(s)).ll
//   }

//   /**
//     * Determines the marginal likelihood of a model given observed data using a recursive function
//     * @param data observed data
//     * @param unparamMod an unparameterised model represented as a function from parameters to model
//     * @param n the total number of particles to use in the particle filter
//     */
//   def pfMll(
//     data: Vector[Data],
//     unparamMod: Parameters => Model)(
//     n: Int): Parameters => LogLikelihood = { p => 

//     val mod = unparamMod(p)
//     val times = data.map(_.t)
//     val dt = diff(times.head +: times)
//     val initState = Vector.fill(n)(mod.x0.draw)

//     def go(
//       observations: Vector[Data],
//       x0: Vector[State],
//       ll: LogLikelihood,
//       deltas: Iterable[Time]): LogLikelihood = observations match {
//       case IndexedSeq() => ll
//       case y +: ys => 
//         val x1 = x0 map (x => mod.stepFunction(x, deltas.head).draw)

//         val likelihoodState = x1 map (x => mod.link(mod.f(x, y.t)))

//         val w1 = likelihoodState map (l => mod.dataLikelihood(l, y.observation))

//         val max = w1.max
//         val w = w1 map { a => exp(a - max) }

//         val xInd = sample(n, DenseVector(w.toArray))
//         val x2 = xInd map ( x1(_) )

//         go(ys, x2, ll + max + log(breeze.stats.mean(w)), deltas.tail)
//     }

//     go(data, initState, 0.0, dt)
//   }

//   def mean(it: ParVector[Double]): Double = {
//     it.sum / it.size
//   }

//   /**
//     * A parallel implementation of the particle filter to calculate pseudo marginal likelihood
//     * @param data a vector of observations
//     * @param unparamMod a function from parameters to model
//     * @param n the number of particles to use in the filter
//     * @return a function from parameters to pseudo marginal loglikelihood
//     */
//   def pfMllPar(
//     data: Vector[Data],
//     unparamMod: Parameters => Model)(
//     n: Int): Parameters => LogLikelihood = { p =>

//     val mod = unparamMod(p)
//     val times = data.map(_.t)
//     val deltas = diff(times.head +: times)
//     val x0: ParVector[State] = Vector.fill(n)(mod.x0.draw).par

//     data.foldLeft((x0, 0.0, deltas))((a, y) => {
//       val (state, ll, dts) = a

//       // Choose next time
//       val dt = dts.head

//       // advance state particles
//       val x1 = state map(x => mod.stepFunction(x, dt).draw)

//       // Calculate the ll of the propagation of particles given the observation
//       val w1 = x1 map (a => mod.dataLikelihood(mod.link(mod.f(a, y.t)), y.observation))

//       val max = w1.max // log-sum-exp trick
//       val w = w1 map { a => exp(a - max) }

//       val xInd = sample(n, DenseVector(w.toArray)).par // Multinomial resampling
//       val x2 = xInd map ( x1(_) )

//       (x2, ll + max + log(mean(w)), dts.tail)
//     })._2
//   }

//   /**
//     * Calculate the importance weights for the Log-Gaussian Cox-Process Bootstrap Filter
//     * @param t is the time of the event
//     * @param t0 is the time of the last event
//     * @param x0 is the state at time t0
//     * @param precision determines the step size of the SDE
//     * @param mod the model
//     * @p the parameters of the model
//     * @return the log likelihood of an event occurring at time t0 given the parameters, the 
//     * state is 
//     */
//   def importanceResampleWeights(
//     t0: Time,
//     t: Time,
//     x0: State,
//     precision: Int,
//     y: Observation,
//     mod: Model): (LogLikelihood, Stream[State]) = {

//     // Step length
//     val dt = Math.pow(10, -precision)

//     // SDE Stream
//     val x = simSdeStream(x0, t0, t - t0, precision, mod.stepFunction).
//       takeWhile(a => a.time <= t)

//     // add deterministic transformation
//     val transformedState = x map (a => mod.f(a.state, a.time))

//     // sum the transformed state to calculate the cumulative hazard and return the value of the state at time t
//     val likelihoodState = Vector(transformedState.last, transformedState.map(x => exp(x) * dt).sum)

//     val likelihood = mod.dataLikelihood(likelihoodState, y)

//     // we return the likelihood of the observation and the entire state sampled between t0 and t
//     (likelihood, x.map(_.state))
//   }

//   def pfLGCPmll(
//     data: Vector[Data],
//     unparamMod: Parameters => Model,
//     precision: Int)(n: Int): Parameters => LogLikelihood = p => {

//     // parameterise the model
//     val mod = unparamMod(p)
//     val particles = Vector.fill(n)(mod.x0.draw)

//     def loop(
//       x0: Vector[State],
//       t0: Time,
//       yy: Vector[Data],
//       ll: LogLikelihood): LogLikelihood = yy match {
//       case IndexedSeq() => ll
//       case y +: ys =>

//         val weightsState = x0 map (x =>
//           importanceResampleWeights(t0, y.t, x, precision, y.observation, mod))

//         val x1 = weightsState map (_._2)
//         val w1 = weightsState map (_._1)

//        // all intermediate states in a vector of streams
//         // we could return these if we wanted to interpolate the state prediction
    
//         // log-sum-exp trick
//         val max = w1.max 
//         val w = w1 map { a => exp(a - max) }

//         // Resample the hazard function according to how likely
//         // it is to have produced an observation at time "t"
//         val xInd = sample(n, DenseVector(w.toArray))
//         val x2: Vector[Stream[State]] = xInd map ( x1(_) )

//         loop(x2.map(_.last), y.t, ys, ll + max + log(breeze.stats.mean(w)))

//     }
//     // initialise with a draw from the model x0 and time 0
//     loop(particles, 0, data, 0)
//   }

//   /** An lgcp particle filter, which utilises streaming in between observations to calculate the cumulative hazard
//     * This could work generically, as a particle filter on a grid (maybe)
//     * This does work generically
//     */
//   def pfLGCP(
//     n: Int,
//     data: Vector[Data],
//     unparamMod: Parameters => Model,
//     precision: Int): Parameters => Vector[PfOut] = p => {

//     val mod = unparamMod(p)
//     val particles = Vector.fill(n)(mod.x0.draw)

//     def loop(
//       x0: Vector[State],
//       t0: Time,
//       yy: Vector[Data],
//       acc: Vector[PfOut]): Vector[PfOut] = yy match {
//       case IndexedSeq() => acc.reverse
//       case y +: ys =>

//         // calculate the importance weights and state from time t0 to y.t stepped by precision
//         val weightsState = x0 map (x =>
//           importanceResampleWeights(t0, y.t, x, precision, y.observation, mod))

//         val x1 = weightsState map (_._2)
//         val w1 = weightsState map (_._1)

//         // log-sum-exp trick
//         val max = w1.max 
//         val w = w1 map { a => exp(a - max) }

//         // Resample the hazard function according to how likely
//         // it is to have produced an observation at time "t"
//         val xInd = sample(n, DenseVector(w.toArray))
//         val x2: Vector[Stream[State]] = xInd map ( x1(_) )

//         val meanState = weightedMean(x2.map(_.last), w)
//         val intervals = getAllCredibleIntervals(x2.map(_.last), 0.995)
//         val eta =  math.exp(mod.f(meanState, y.t))
//         val etaIntervals = getOrderStatistic(x2.map(_.last).map(a => math.exp(mod.f(a, y.t))), 0.995)

//         loop(x2.map(_.last), y.t, ys,
//           PfOut(y.t, Some(1.0), eta, etaIntervals, meanState, intervals) +: acc)

//     }
//     // initialise with a draw from the model x0 and time 0
//     loop(particles, 0, data, Vector())
//   }

// }
