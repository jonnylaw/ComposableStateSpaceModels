package com.github.jonnylaw.model

import breeze.stats.distributions.{Rand, Uniform, Multinomial}
import breeze.stats.distributions.Rand._
import breeze.numerics.{exp, log}
import breeze.linalg.DenseVector

import cats._
import cats.data.Reader
import cats.implicits._

import fs2._

import scala.collection.parallel.immutable.ParVector

/**
  * Representation of the state of the particle filter, at each step the previous observation time, t0, and 
  * particle cloud, particles, is required to compute forward.
  * The meanState and intervals are recorded in each step, so they can be outputted immediately without having
  * to calculate these from the particle cloud after
  */
case class PfState(
  t: Time,
  observation: Option[Observation],
  particles: Seq[State],
  weights: Seq[LogLikelihood],
  ll: LogLikelihood)

/**
  * A class representing a return type for the particle filter, containing the state and associated credible intervals
  * @param time the time of the process
  * @param observation an optional observation, note discretely observed processes cannot be seen at all time points continuously
  * @param state the mean of the empirical filtering distribution at time 'time'
  * @param intervals the credible intervals of the filtering distribution
  */
case class PfOut(
  time: Time,
  observation: Option[Observation],
  eta: Double,
  etaIntervals: CredibleInterval,
  state: State,
  stateIntervals: IndexedSeq[CredibleInterval])

/**
  * Forecast data
  * @param t the time of the observation
  * @param obs an observation of the process
  * @param obsIntervals the upper and lower credible intervals of the observation
  * @param eta the transformed latent state
  * @param etaIntervals the credible intervals of the transformed latent state
  * @param state the untransformed latent state
  * @param stateIntervals the intervals of the latent state
  */
case class ForecastOut(
  t: Time,
  obs: Observation,
  obsIntervals: CredibleInterval,
  eta: Double,
  etaIntervals: CredibleInterval,
  state: State,
  stateIntervals: IndexedSeq[CredibleInterval])


trait ParticleFilter {
  import ParticleFilter._
  /**
    * A model
    */
  val mod: Model

  def initialiseState(particles: Int, t0: Time): PfState = {
    val state = mod.sde.initialState.sample(particles)
    PfState(t0, None, state, Seq.fill(particles)(1.0), 0.0)
  }

  def advanceState(x: Seq[State], dt: TimeIncrement, t: Time): Seq[(State, Eta)]

  def calculateWeights(x: Eta, y: Observation): LogLikelihood

  def resample: Resample[State]

  /**
    * Step filter, perform one step of the particle fiilter
    */
  def stepFilter(s: PfState, y: Data): PfState = {
    val dt = y.t - s.t // calculate time between observations

    val unweightedX: Seq[State] = resample(s.particles, s.weights)

    val (x1, eta) = advanceState(unweightedX, dt, y.t).unzip
    val w = eta map (a => calculateWeights(a, y.observation))
    val max = w.max
    val w1 = w map { a => exp(a - max) }

    val ll = s.ll + max + log(breeze.stats.mean(w1))

    PfState(y.t, Some(y.observation), x1, w1, ll)
  }

  def stepWithForecast(s: PfState, y: Data): (PfState, ForecastOut) = {
    val dt = y.t - s.t // calculate time between observations

    val unweightedX: Seq[State] = resample(s.particles, s.weights)

    val (x1, eta) = advanceState(unweightedX, dt, y.t).unzip

    val stateIntervals = getAllCredibleIntervals(x1, 0.995)
    val statemean = meanState(x1)
    val meanEta = breeze.stats.mean(eta.map(_.head))
    val etaIntervals = getOrderStatistic(eta.map(_.head), 0.995)
    val obs = breeze.stats.mean(eta map (mod.observation(_).draw))
    val obsIntervals = getOrderStatistic(eta map (mod.observation(_).draw), 0.995)

    val fo = ForecastOut(y.t, obs, obsIntervals, meanEta, etaIntervals, statemean, stateIntervals)

    val w = eta map (a => calculateWeights(a, y.observation))
    val max = w.max
    val w1 = w map { a => exp(a - max) }

    val ll = s.ll + max + log(breeze.stats.mean(w1))

    val pf = PfState(y.t, Some(y.observation), x1, w1, ll)

    (pf, fo)
  }

  /**
    * Calculate the log-likelihood
    */
  def llFilter(t0: Time, particles: Int)(data: Seq[Data]): LogLikelihood = {
    val initState = initialiseState(particles, t0)
    data.foldLeft(initState)(stepFilter).ll
  }

  /**
    * Run a filter over a vector of data and return a vector of PfState
    * Containing the raw particles and associated weights at each time step
    */
  def accFilter(data: Seq[Data], t0: Time)(particles: Int): Seq[PfState] = {
    val initState = initialiseState(particles, t0)

    val x = data.scanLeft(initState)(stepFilter)

    x.tail
  }

  /**
    * Filter the data, but get a vector containing the mean eta, eta intervals, mean state, 
    * and credible intervals of the state
    */
  def filterWithIntervals(data: Seq[Data], t0: Time)(particles: Int): Seq[PfOut] = {
    accFilter(data, t0)(particles).map(getIntervals(mod))
  }

  /**
    * Run a filter over a stream of data
    */
  def filter(t0: Time)(particles: Int): Pipe[Task, Data, PfState] = {
    val initState = initialiseState(particles, t0)

    pipe.scan(initState)(stepFilter)
  }

  /**
    * One step forecast filter, advances the particles ahead to the time of the next observation
    * transforms them according to the model and calculates the expected observation and 99% 
    * credible intervals
    * @param t0 the initial time of the observations
    * @param particles the number of particles to use in the filter/forecast
    * @return a Pipe[Task, Data, (PfState, ForecastOut)], which transforms a stream of Data into a stream
    * of tuples (PfState, ForecastOut)
    */
  def filterWithForecast(t0: Time)(particles: Int): Pipe[Task, Data, (PfState, ForecastOut)] = {
    val initState = initialiseState(particles, t0)
    val forecastState = oneStepForecast(initState.particles.toList, t0, t0, mod).draw._1

    pipe.scan((initState, forecastState))((s: (PfState, ForecastOut), y: Data) => stepWithForecast(s._1, y))
  }

  def initStatePar(n: Int, t: Time) = {
    (t, ParVector.fill(n)(mod.sde.initialState.draw), 0.0)
  }

  /**
    * A datatype-generic mean function, can we shorten this with Spire?
    */
  def parMean[A](s: ParVector[A])(implicit N: Fractional[A]): A = {
    N.div(s.sum, N.fromInt(s.size))
  }

  def stepFilterPar(y: Data) = { (t0: Time, x: ParVector[State], ll: LogLikelihood) =>
    val (s, w) = (for {
      state <- x
      x1 = mod.sde.stepFunction(y.t - t0)(state).draw
      eta = mod.link(mod.f(x1, y.t))
      weight = mod.dataLikelihood(eta, y.observation)
    } yield (x1, weight)).unzip

    val max = w.max
    val w1 = w map { a => exp(a - max) }
    val newLl = ll + max + log(parMean(w1))

    (y.t, ParticleFilter.parallelSystematicResampling(s, w1), newLl)
  }

  def llPar(t0: Time, particles: Int)(data: Seq[Data]): LogLikelihood = {
    val initState = initStatePar(particles, t0)
    data.foldLeft(initState)((s, d) => stepFilterPar(d)(s._1, s._2, s._3))._3
  }

}

case class Filter(mod: Model, resamplingScheme: Resample[State]) extends ParticleFilter {
  
  def advanceState(states: Seq[State], dt: TimeIncrement, t: Time): Seq[(State, Eta)] = {
    for {
      x <- states
      x1 = mod.sde.stepFunction(dt)(x).draw
      eta = mod.link(mod.f(x1, t))
    } yield (x1, eta)
  }

  def calculateWeights(x: Eta, y: Observation): LogLikelihood = {
    mod.dataLikelihood(x, y)
  }

  def resample: Resample[State] = resamplingScheme
}

/**
  * Credible intervals from a set of samples in a distribution
  * @param lower the lower interval
  * @param upper the upper interval
  */
case class CredibleInterval(lower: Double, upper: Double) {
  override def toString = lower + ", " + upper
}

/**
  * In order to calculate Eta in the LGCP model, we need to merge the advance state and transform state functions
  */
case class FilterLgcp(mod: Model, resamplingScheme: Resample[State], precision: Int) extends ParticleFilter {

  def calcWeight(x: State, dt: TimeIncrement, t: Time): (State, Eta) = {
    val x1 = SimulateData.simSdeStream(x, t - dt, dt, precision, mod.sde.stepFunction)
    val transformedState = x1 map (a => mod.f(a.state, a.time))

    (x1.last.state, Seq(transformedState.last, transformedState.map(x => exp(x) * dt).sum))
  }

  def advanceState(x: Seq[State], dt: TimeIncrement, t: Time): Seq[(State, Eta)] = {
    x map (calcWeight(_, dt, t))
  }

  def calculateWeights(x: Eta, y: Observation): LogLikelihood = {
    mod.dataLikelihood(x, y)
  }

  def resample: Resample[State] = resamplingScheme
}

object ParticleFilter {
  /**
    * Construct a particle filter to calculate the state of an fs2 Stream of Data
    * @param t0 the starting time of the observations
    * @param n the number of particles to use in the filter
    * @return a Reader monad representing the function Model => Pipe[Task, Data, PfState]
    * When given a Model, this can be used to filter an fs2 stream of data
    * eg:
    * val mod: Model
    * val pf = filter(0.0, 100)
    * val data: Stream[Task, Data] = // data as an fs2 stream
    * data.
    *   through(pf(mod))
    */
  def filter(t0: Time, n: Int): Reader[Model, Pipe[Task, Data, PfState]] = Reader {
    mod => Filter(mod, ParticleFilter.multinomialResampling).filter(t0)(n)
  }

  /**
    * Construct a particle filter to determine the pseudo-marginal likelihood 
    * of a POMP model
    * This function returns the Reader Monad (Reader[Model, LogLikelihood]) which means the function
    * can be composed with a model:
    * val mod: Reader[Parameters, Model] = // any model here
    * val mll: Reader[Parameters, Likelihood] = likelihood(_, _, _) compose mod
    * To get the function back from inside the Reader monad, simply use mll.run
    * @param resamplingScheme the resampling scheme to use in the particle filter
    * @param data a sequence of data to determine the likelihood of
    * @param n the number of particles to use in the particle filter
    * @return a function from model to logLikelihood
    */
  def likelihood(
    resamplingScheme: Resample[State],
    data: Seq[Data],
    n: Int): Reader[Model, LogLikelihood] = Reader {
    mod => Filter(mod, resamplingScheme).
      llFilter(data.map((d: Data) => d.t).min, n)(data.sortBy((d: Data) => d.t))
  }

  def parLikelihood(
    data: Seq[Data],
    n: Int): Reader[Model, LogLikelihood] = Reader {
    mod => Filter(mod, ParticleFilter.multinomialResampling).
      llPar(data.map((d: Data) => d.t).min, n)(data.sortBy((d: Data) => d.t))
  }

  /**
    * Transforms PfState into PfOut, including eta, eta intervals and state intervals
    */
  def getIntervals(mod: Model): PfState => PfOut = s => {
    val meanState = weightedMean(s.particles, s.weights)
    val stateIntervals = getAllCredibleIntervals(s.particles, 0.995)
    val etas = s.particles map (x => mod.link(mod.f(x, s.t)).head)
    val meanEta = mod.link(mod.f(meanState, s.t)).head
    val etaIntervals = getOrderStatistic(etas, 0.995)

    PfOut(s.t, s.observation, meanEta, etaIntervals, meanState, stateIntervals)
  }

  /**
    * Return a vector of lag 1 time differences
    * @param x a list of times
    * @return a list of differenced times
    */
  def diff(x: Iterable[Time]): Iterable[TimeIncrement] = {
    (x.tail zip x) map { a => a._1 - a._2 }
  }

  /**
    * Sample integers from 1 to n with replacement according to their associated probabilities
    * @param n a number matching the number of probabilities
    * @param prob a vector of probabilities corresponding to the probability of sampling that integer
    * @return a vector containing the samples
    */
  def sample(n: Int, prob: DenseVector[Double]): Seq[Int] = {
    Multinomial(prob).sample(n).toSeq
  }

  /**
    * Given a vector of doubles, returns a normalised vector with probabilities summing to one
    * @param prob a vector of unnormalised probabilities
    * @return a vector of normalised probabilities
    */
  def normalise(prob: Seq[Double]): Seq[Double] = {
    prob map (_/prob.sum)
  }

  /**
    * Calculate the cumulative sum of a sequence
    */
  def cumsum(x: Seq[Double]): Seq[Double] = {
    x.scanLeft(0.0)(_ + _)
  }

  /**
    * Multinomial Resampling, sample from a categorical distribution with probabilities
    * equal to the particle weights 
    */
  def multinomialResampling[A](particles: Seq[A], weights: Seq[LogLikelihood]): Seq[A] = {
    val indices = sample(particles.size, DenseVector(weights.toArray))
    indices map { particles(_) }
  }

  /**
    * Calculate the effective sample size of a particle cloud
    */
  def effectiveSampleSize(weights: Seq[LogLikelihood]): Int = {
    math.floor(1 / (weights, weights).zipped.map(_ * _).sum).toInt
  }

  /**
    * Produces a histogram output of a vector of Data
    */
  def hist(x: Seq[Int]): Unit = {
    val h = x.
      groupBy(identity).
      toArray.
      map{ case (n, l) => (n, l.length) }.
      sortBy(_._1)

    h foreach { case (n, count) => println(s"$n: ${Seq.fill(count)("#").mkString("")}") }
  }

  /**
    * Return the value x such that, F(p) = x, where F is the empirical cumulative distribution function over
    * the particles
    */
  def invecdf[A](ecdf: Seq[(A, LogLikelihood)], p: Double): A = {
    ecdf.
      filter{ case (_, w) => w > p }.
      map{ case (x, _) => x }.
      head
  }

  /**
    * Stratified resampling
    * Sample n ORDERED uniform random numbers (one for each particle) using a linear transformation of a U(0,1) RV
    */
  def stratifiedResampling[A](particles: Seq[A], weights: Seq[LogLikelihood]): Seq[A] = {
    // generate n uniform random numbers
    val n = weights.length
    val u = (1 to n).map(k => (k - 1 + Uniform(0,1).draw) / n).toSeq
    val ecdf = particles.zip(cumsum(normalise(weights)))

    u map (invecdf(ecdf, _))
  }

  /**
    * Systematic Resampling
    * Sample n ORDERED numbers (one for each particle), reusing the same U(0,1) variable
    */
  def systematicResampling[A](particles: Seq[A], weights: Seq[LogLikelihood]): Seq[A] = {
    val n = weights.length
    val u = Uniform(0,1).draw
    val k = (1 to n).map(a => (a - 1 + u) / n).toSeq
    val ecdf = particles.zip(cumsum(normalise(weights)))

    k map (invecdf(ecdf, _))
  }

  /**
    * Parallel systematic resampling
    */
  def parallelSystematicResampling[A](s: ParVector[A], w: ParVector[Double]): ParVector[A] = {
    val u = scala.util.Random.nextDouble // generate a random number 
    val n = s.size
    val k = ParVector.range(1, n).map(i => (i - 1 + u) / n)

    val ecdf = w.
      map(_ / w.sum).
      scanLeft(0.0)(_+_)

    val indices = k.map { i => ecdf.indexWhere(e => e > i ) }

    indices.map(i => s(i-1))
  }

  /**
    * Residual Resampling
    * Select particles in proportion to their weights, ie particle xi appears ki = n * wi times
    * Resample m (= n - total allocated particles) particles according to w = n * wi - ki using other resampling technique
    */
  def residualResampling[A](particles: Seq[A], weights: Seq[LogLikelihood]): Seq[A] = {
    val n = weights.length
    val normalisedWeights = normalise(weights)
    val ki = normalisedWeights.
      map (w => math.floor(w * n).toInt)

    val indices = ki.zipWithIndex.
      map { case (n, i) => Seq.fill(n)(i) }.
      flatten
    val x = indices map { particles(_) }
    val m = n - indices.length
    val residualWeights = normalisedWeights.zip(ki) map { case (w, k) => n * w - k }

    val i = sample(m, DenseVector(residualWeights.toArray))
    x ++ (i map { particles(_) })
  }

  /**
    * Gets credible intervals for a vector of doubles
    * @param samples a vector of samples from a distribution
    * @param interval the upper interval of the required credible interval
    * @return order statistics representing the credible interval of the samples vector
    */
  def getOrderStatistic(samples: Seq[Double], interval: Double): CredibleInterval = {
    val index = math.floor(samples.length * interval).toInt
    val ordered = samples.sorted

    CredibleInterval(ordered(samples.length - index), ordered(index))
  }

  def indentity[A](samples: Seq[A], weights: Seq[LogLikelihood]) = samples

  def oneStepForecast(
    x0: List[State], t0: Time, t: Time, mod: Model): Rand[(ForecastOut, Seq[State])] = {

    for {
      x1 <- x0 traverse mod.sde.stepFunction(t - t0)
      stateIntervals = getAllCredibleIntervals(x1, 0.995)
      statemean = meanState(x1)
      etas = x1 map (x => mod.link(mod.f(x, t)))
      meanEta = breeze.stats.mean(etas.map(_.head))
      etaIntervals = getOrderStatistic(etas.map(_.head), 0.995)
      obs = breeze.stats.mean(etas map (mod.observation(_).draw))
      obsIntervals = getOrderStatistic(etas map (mod.observation(_).draw), 0.995)
    } yield
      (ForecastOut(t, obs, obsIntervals, meanEta, etaIntervals, statemean, stateIntervals), x1)
  }


  /**
    * Calculate the weighted mean of a particle cloud
    */
  def weightedMean(x: Seq[State], w: Seq[Double]): State = {
    val normalisedWeights = w map (_ / w.sum)

    x.zip(normalisedWeights).
      map { case (state, weight) => state map ((x: DenseVector[Double]) => x * weight) }.
      reduce((a, b) => a.add(b))
  }

  /**
    *  Calculate the mean of a state
    */
  def meanState(x: Seq[State]): State = {
    weightedMean(x, Seq.fill(x.length)(1.0))
  }

  /**
    * Get the credible intervals of the nth state vector
    * @param s a State
    * @param n a reference to a node of state tree, counting from 0 on the left
    * @param interval the probability interval size
    * @return a tuple of doubles, (lower, upper)
    */
  def getCredibleInterval(
    s: Seq[State],
    n: Int,
    interval: Double): IndexedSeq[CredibleInterval] = {

    val state = s.map(_.getNode(n))  // Gets the nth state vector

    val stateVec = state.head.data.indices map (i => state.map(a => a(i)))

    stateVec map (a => {
      val index = Math.floor(interval * a.length).toInt
      val stateSorted = a.sorted
      CredibleInterval(stateSorted(a.length - index - 1), stateSorted(index - 1))
    })
  }

  /**
    * Use getCredibleInterval to get all credible intervals of a state
    * @param s a vector of states
    * @param interval the interval for the probability interval between [0,1]
    * @return a sequence of tuples, (lower, upper) corresponding to each state reading
    */
  def getAllCredibleIntervals(s: Seq[State], interval: Double): IndexedSeq[CredibleInterval] = {
    s.head.flatten.indices.flatMap(i => getCredibleInterval(s, i, interval))
  }


  /**
    * Given a distribution over State, calculate credible intervals by 
    * repeatedly drawing from the distribution and ordering the samples
    */
  def getCredibleIntervals(x0: Rand[State], interval: Double): IndexedSeq[CredibleInterval] = {
    getAllCredibleIntervals(x0.sample(1000), interval)
  }
}
