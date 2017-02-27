package com.github.jonnylaw.model

import akka.stream.scaladsl._
import akka.stream._
import akka.NotUsed

import breeze.stats.distributions.{Rand, Multinomial}
import breeze.numerics.{exp, log}
import breeze.linalg.DenseVector

import cats._
import cats.data.Reader
import cats.implicits._
import cats.instances._

import scala.collection.parallel.immutable.ParVector
import scala.collection.immutable.TreeMap
import scala.concurrent._
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.ClassTag
import simulacrum._
import Collection.ops._

/**
  * A typeclass representing a Collection with a few additional 
  * features required for implementing the particle filter
  */
@typeclass trait Collection[F[_]] {
  def pure[A](a: A): F[A]
  def isEmpty[A](fa: F[A]): Boolean
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def scanLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): F[B]
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  def get[A](fa: F[A])(i: Int): A
  def indices[A](fa: F[A]): F[Int]
  def indexWhere[A](fa: F[A])(cond: A => Boolean): Int
  def max[A: Ordering](fa: F[A]): A
  def size[A](fa: F[A]): Int
  def fill[A](n: Int)(a: A): F[A]
  def toArray[A: ClassTag](fa: F[A]): Array[A]
  def toVector[A](fa: F[A]): Vector[A]
  def unzip[A, B](fa: F[(A, B)]): (F[A], F[B])
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def drop[A](fa: F[A])(n: Int): F[A]
  def toTreeMap[A: Ordering, B](fa: F[(A, B)]): TreeMap[A, B]
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
  * Representation of the state of the particle filter, where the particles are in a Collection typeclass 
  * defined in package.scala
  * @param t the time of the current observation
  * @param observation the current observation at time t
  * @param particles a collection containing an approximate sample from the filtering distribution p(x(t) | y(t0:t))
  * @param ll the estimated log-likelihood of the path given the observations so far
  * @param ess the effective sample size 
  */
case class PfState[F[_]](
  t: Time,
  observation: Option[Observation],
  particles: F[State],
  ll: LogLikelihood, 
  ess: Int)

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
  * @param gamma the transformed latent state
  * @param gammaIntervals the credible intervals of the transformed latent state
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

/**
  * This class represents the state of a a filter starting with a draw from the joint posterior p(x, theta | y)
  * as the filter is applied, the value of the state changes with time, but the parameters, theta, are static
  * @param t the time of the draw from the joint posterior
  * @param observation the measurement taken at time t
  * @param ps a tuple containing the parameter and state drawn from the joint posterior
  */
case class ParamFilterState[F[_]](t: Time, observation: Option[Observation], ps: F[(Parameters, State)], ll: LogLikelihood)

trait ParticleFilter[F[_], G[_]] {
  implicit val f: Collection[F]
  implicit val g: Monad[G]

//  implicit val ec: ExecutionContext
  import ParticleFilter._

  val mod: Model

  def initialiseState(particles: Int, t0: Time): PfState[F] = {
    val state = f.fill(particles)(mod.sde.initialState.draw)
    PfState[F](t0, None, state, 0.0, particles)
  }

  def resample: Resample[F, G, State]

  def stepFilter(s: PfState[F], y: Data): G[PfState[F]] = {
    val dt = y.t - s.t
    val x1 = s.particles map (x => mod.sde.stepFunction(dt)(x).draw)
    val w = x1 map (x => mod.dataLikelihood(mod.f(x, y.t), y.observation))
    val max = w.max
    val w1 = w map (a => exp(a - max))
    val ll = s.ll + max + log(ParticleFilter.mean(w1))
    val ess = ParticleFilter.effectiveSampleSize(w1)
    
    for {
      resampledX <- resample(x1, w1)
    } yield PfState(y.t, Some(y.observation), resampledX, ll, ess)
  }

  /**
    * Filter a collection of data and return an estimate of the loglikelihood
    */
  def llFilter(data: Vector[Data])(n: Int): G[LogLikelihood] = {
    val initState = g.pure(initialiseState(n, data.minBy(_.t).t))
    data.foldLeft(initState)((s, y) => s flatMap(stepFilter(_, y))).
      map(_.ll)
  }

  /**
    * A particle filter to be ran over observed data to return the log-likelihood and a proposed value
    * of the path by sampling from the distribution of the paths
    * @param data the initial time of the data
    * @param particles the number of particles to use in the particle approximation to the filtering distribution
    * @return (LogLikelihood, Vector[State]) The log likelihood and a sample from the posterior of the filtering distribution
    */
  def filter(data: Vector[Data])(particles: Int): G[(LogLikelihood, Vector[State])] = ???

// {
//     val init = g.pure(initialiseState(particles, data.minBy(_.t).t))

//     for {
//       states: Vector[PfState[F]] <- data.scanLeft(init)((s, y) => s flatMap(stepFilter(_, y)))
//       ll = states.last.ll
//     } yield (ll, Resampling.sampleOne(states.map(_.particles)).toVector)
//   }

  /**
    * Run a filter over a stream of data
    */
  def filterStream(t0: Time)(particles: Int): Flow[Data, G[PfState[F]], NotUsed] = {
    val initState = g.pure(initialiseState(particles, t0))

    Flow[Data].scan(initState)((s, y) => s flatMap(stepFilter(_, y)))
  }
}

case class FilterLgcp(
  mod: Model,
  resample: Resample[ParVector, Id, State], 
  precision: Int) extends ParticleFilter[ParVector, Id] {

  override val f = implicitly[Collection[ParVector]]
  override val g = implicitly[Monad[Id]]

  def calcWeight(x: State, dt: TimeIncrement, t: Time): (State, Double, Double) = {
    // calculate the amount of realisations of the state we need
    val n = Math.ceil((dt) / Math.pow(10, -precision)).toInt

    // build a stream capable of materializing all the values of the state
    val x1: Stream[StateSpace] = mod.sde.simInitStream(t, x, Math.pow(10, -precision)).take(n)

    // get the value in the last position of the stream
    val lastState = x1.last.state

    // transform the state
    val gamma = mod.f(lastState, t)

    // calculate the cumulative hazard
    val cumulativeHazard = x1.map((a: StateSpace) => mod.f(a.state, a.time)).
      map(x => exp(x) * Math.pow(10, -precision)).
      fold(0.0)(_+_)

    (lastState, gamma, cumulativeHazard)
  }

  override def stepFilter(s: PfState[ParVector], y: Data): PfState[ParVector] = {
    val dt = y.t - s.t
    val state = if (dt == 0) { s.particles map (x => (x, mod.f(x, y.t), mod.f(x, y.t))) } else { s.particles.map(x => calcWeight(x, dt, y.t)) }
    val w = state.map(a => (a._2, a._3)).map { case (g,l) => g - l }
    val max = w.max
    val w1 = w map (a => exp(a - max))
    val ll = s.ll + max + log(ParticleFilter.mean(w1))

    val resampledX = resample(state.map(_._1), w1)
    val ess = ParticleFilter.effectiveSampleSize(w1)

    PfState[ParVector](y.t, Some(y.observation), resampledX, ll, ess)
  }
}

case class FilterAsync(
  parallelism: Int, 
  mod: Model, 
  resample: Resample[Vector, Future, State])(
  implicit ec: ExecutionContext) extends ParticleFilter[Vector, Future] {

  override val f = implicitly[Collection[Vector]]
  override val g = implicitly[Monad[Future]]

  override def stepFilter(s: PfState[Vector], y: Data): Future[PfState[Vector]] = {
    val dt = y.t - s.t

    // advance the state and calculate the likelihood in parallel
    // by transforming the vectors into ParVectors
    val newState = s.particles.par map (x => mod.sde.stepFunction(dt)(x).draw)
    val weight = newState.par map (x => mod.dataLikelihood(mod.f(x, y.t), y.observation))

    // update the value of the log-likelihood, and effective sample size
    val max = weight.max
    val w1 = weight map (a => exp(a - max))
    val ll = s.ll + max + log(ParticleFilter.mean(w1))
    val ess = ParticleFilter.effectiveSampleSize(w1)

    // resample using Future async
    for {
      resampledX <- resample(newState.toVector, w1.toVector)
    } yield PfState(y.t, Some(y.observation), resampledX, ll, ess)
  }

}

/**
  * A particle filter which can represent particle clouds as a Collection, Collection is a typeclass
  * which can currently has concrete methods for Vector and ParVector 
  * @param unparamModel
  */
case class Filter[F[_]: Collection, G[_]: Monad](
  mod: Model, 
  resample: Resample[F, G, State]) extends ParticleFilter[F, G] {

  override val f = implicitly[Collection[F]]
  override val g = implicitly[Monad[G]]
}

/**
  * A particle filter which takes a sample from the joint-posterior of the parameters
  * and state p(x, theta | y) 
  * @param 
  */
case class FilterState[F[_]: Collection](
  unparamModel: Parameters => Model, 
  resample: Resample[F, Id, (Parameters, State)]) {

  val f = implicitly[Collection[F]]

  /**
    * Apply the particle filter with a draw from the joint posterior of the state and parameters, p(x, theta | y)
    */
  def stepFilterParameters = { (s: ParamFilterState[F], y: Data) =>

    val dt = y.t - s.t
    val x1 = f.map(s.ps){ case (p, x) => (p, unparamModel(p).sde.stepFunction(dt)(x).draw) }
    val w = f.map(x1){ case (p, x) => unparamModel(p).dataLikelihood(unparamModel(p).f(x, y.t), y.observation) }
    val max = f.max(w)
    val w1 = f.map(w)(a => exp(a - max))
    val ll = s.ll + max + log(ParticleFilter.mean(w1))
    val resampledX = resample(x1, w1)
    val ess = ParticleFilter.effectiveSampleSize(w1)
  
    ParamFilterState[F](y.t, Some(y.observation), resampledX, ll)
  }

  /**
    * Filter data, initialised with a draw from the joint-posterior of the state and parameters p(x, theta | y) at time t
    * @param t the starting time of the filter, the initial state is assumed to be sampled from this time.
    * The initial state is then advanced to the time of the next observation
    * @param init a draw from the full-joint posterior distribution at time t, output from the PMMH
    */
  def filterFromPosterior(t: Time)(init: F[(Parameters, State)]) = {

    val initState = ParamFilterState[F](t, None, init, 0.0)

    Flow[Data].scan(initState)(stepFilterParameters)
  }

}

object ParticleFilter {
  /**
    * Construct a particle filter to calculate the state of an Akka Stream of Data
    * @param t0 the starting time of the observations
    * @param n the number of particles to use in the filter
    * @return a Reader monad representing the function Model => Flow[Task, Data, PfState]
    * When given a Model, this can be used to filter an fs2 stream of data
    * eg:
    * val mod: Model
    * val resample: Resample[F, G, State]
    * val pf = filter(resample, 0.0, 100)
    * val data: Source[NotUsed, Data] = // data as an fs2 stream
    * data.
    *   through(pf(mod))
    */
  def filter[F[_]: Collection](
    resample: Resample[F, Id, State],
    t0: Time,
    n: Int
  ) = Reader { (mod: Model) =>

    Filter[F, Id](mod, resample).filterStream(t0)(n)
  }

  /**
    * Construct a particle filter which starts at time t with a draw from the joint posterior p(x, theta | y)
    * at time t
    * @param resample the resampling scheme
    * @param t the time to start the filter
    * @param initState a collection of particles representing the state of the 
    */
  def filterFromPosterior[F[_]: Collection](
    resample: Resample[F, Id, (Parameters, State)], 
    init: F[(Parameters, State)],
    t: Time)(unparamModel: Parameters => Model) = {

    FilterState[F](unparamModel, resample).filterFromPosterior(t)(init)
  }

  /**
    * Return the likelihood and a sample from the state path as a tuple
    * @param data a vector containing observations
    * @param resample a method of resampling in the particle filter
    * @param n the number of particles to use in the particle filter
    * @param model an unparameterised model
    */
  def filterLlState[F[_]: Collection](
    data: Vector[Data],
    resample: Resample[F, Id, State],
n: Int
  ) = Reader { (mod: Model) =>

    Filter[F, Id](mod, resample).filter(data)(n)
  }

  /**
    * Construct a particle filter to determine the pseudo-marginal likelihood 
    * of a POMP model
    * @param data a sequence of data to determine the likelihood of
    * @param n the number of particles to use in the particle filter
    * @param model an unparameterised model to use in the filter
    * @param parameters the starting parameters of the filter
    * @return a value of logLikelihood
    */
  def likelihood[F[_]: Collection](data: Vector[Data], 
    resample: Resample[F, Id, State], n: Int) = Reader { (model: Model) =>

    Filter[F, Id](model, resample).
      llFilter(data.sortBy((d: Data) => d.t))(n)
  }

  /**
    * Given an initial set of particles, representing an approximation of the posterior
    * distribution of the filtering state at time t0, simulate the particles forward
    * calculating the predicted observation distribution and intervals
    */
  def getForecast[F[_]](
    s: PfState[F], 
    mod: Model, 
    t: Time)(implicit f: Collection[F]): F[ObservationWithState] = {
    val dt = t - s.t

    f.map(s.particles){ state =>
      val x1 = mod.sde.stepFunction(dt)(state).draw
      val gamma = mod.f(x1, t)
      val eta = mod.link(gamma)
      val obs = mod.observation(gamma)
      ObservationWithState(t, obs.draw, eta, gamma, x1)
    }
  }

  def summariseForecast(mod: Model, interval: Double): Vector[ObservationWithState] => ForecastOut = { forecast =>

    val stateIntervals = getallCredibleIntervals(forecast.map(_.sdeState).toVector, 0.995)
    val statemean = meanState(forecast.map(_.sdeState).toVector)
    val meanEta = breeze.stats.mean(forecast.map(_.eta))
    val etaIntervals = getOrderStatistic(forecast.map(_.eta).toVector, 0.995)
    val obs = forecast.map(x => mod.observation(x.gamma).draw)
    val obsIntervals = getOrderStatistic(obs, 0.995)

    ForecastOut(forecast.head.t, breeze.stats.mean(obs), obsIntervals, 
      meanEta, etaIntervals, statemean, stateIntervals)
  }

  /**
    * Given a state of the particle filter, advance the state and calculate the mean 
    * of the state, gamma and forecast observation
    * @param s the state of the particle filter
    * @param mod the model used to predict the observations
    * @param t the time of the prediction
    * @return ForecastOut, a summary containing the mean of the state, gamma and observation
    */
  def getMeanForecast[F[_]](
    s: PfState[F], 
    mod: Model, 
    t: Time)(implicit f: Collection[F]): ForecastOut = {

    val forecast = f.toVector(getForecast[F](s, mod, t))

    val stateIntervals = getallCredibleIntervals(forecast.map(_.sdeState).toVector, 0.995)
    val statemean = meanState(forecast.map(_.sdeState).toVector)
    val meanEta = breeze.stats.mean(forecast.map(_.eta))
    val etaIntervals = getOrderStatistic(forecast.map(_.eta).toVector, 0.995)
    val obs = forecast.map(x => mod.observation(x.gamma).draw)
    val obsIntervals = getOrderStatistic(obs, 0.995)

    ForecastOut(t, breeze.stats.mean(obs), obsIntervals, 
      meanEta, etaIntervals, statemean, stateIntervals)
  }

  /**
    * Transforms PfState into PfOut, including gamma, gamma intervals and state intervals
    */
  def getIntervals[F[_]](mod: Model)(implicit f: Collection[F]): PfState[F] => PfOut = s => {
    val state = f.toVector(s.particles)
    val stateMean = meanState(state)
    val stateIntervals = getallCredibleIntervals(state, 0.975)
    val gammas = state map (x => mod.link(mod.f(x, s.t)))
    val meanGamma = mod.link(mod.f(stateMean, s.t))
    val gammaIntervals = getOrderStatistic(gammas, 0.975)

    PfOut(s.t, s.observation, meanGamma, gammaIntervals, stateMean, stateIntervals)
  }


  /**
    * Calculate the effective sample size of a particle cloud, from the un-normalised weights
    * by first normalising the weights, then calculating the reciprocal of the sum of the squared weights
    * @param weights the unnormalised weights
    */
  def effectiveSampleSize[F[_]](weights: F[Double])(implicit f: Collection[F]): Int = {
    val normalisedWeights = Resampling.normalise(weights)
    math.floor(1 / sum(f.map(normalisedWeights)(w => w * w))).toInt
  }

  /**
    * Produces a histogram output of a vector of Data
    */
  def hist(x: Vector[Int]): Unit = {
    val h = x.
      groupBy(identity).
      toArray.
      map{ case (n, l) => (n, l.length) }.
      sortBy(_._1)

    h foreach { case (n, count) => println(s"$n: ${Vector.fill(count)("#").mkString("")}") }
  }


  /**
    * Gets credible intervals for a vector of doubles
    * @param samples a vector of samples from a distribution
    * @param interval the upper interval of the required credible interval
    * @return order statistics representing the credible interval of the samples vector
    */
  def getOrderStatistic(samples: Vector[Double], interval: Double): CredibleInterval = {
    val index = math.floor(samples.size * interval).toInt
    val ordered = samples.sorted

    CredibleInterval(ordered(samples.size - index), ordered(index))
  }

  def indentity[A](samples: Vector[A], weights: Vector[Double]) = samples

  /**
    * Calculate the weighted mean of a particle cloud
    */
  def weightedMean(x: Vector[State], w: Vector[Double]): State = {
    val normalisedWeights = w map (_ / w.sum)

    x.zip(normalisedWeights).
      map { case (state, weight) => state map ((x: DenseVector[Double]) => x * weight) }.
      reduce((a, b) => a.add(b))
  }

  /**
    *  Calculate the mean of a state
    */
  def meanState(x: Vector[State]): State = {
    weightedMean(x, Vector.fill(x.length)(1.0))
  }

  /**
    * Get the credible intervals of the nth state vector
    * @param s a State
    * @param n a reference to a node of state tree, counting from 0 on the left
    * @param interval the probability interval size
    * @return a tuple of doubles, (lower, upper)
    */
  def getCredibleInterval(
    s: Vector[State],
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
  def getallCredibleIntervals(s: Vector[State], interval: Double): IndexedSeq[CredibleInterval] = {
    s.head.flatten.indices.flatMap(i => getCredibleInterval(s, i, interval))
  }

  /**
    * Given a distribution over State, calculate credible intervals by 
    * repeatedly drawing from the distribution and ordering the samples
    */
  def getCredibleIntervals(x0: Rand[State], interval: Double): IndexedSeq[CredibleInterval] = {
    getallCredibleIntervals(x0.sample(1000).toVector, interval)
  }

  def sum[F[_], A](l: F[A])(implicit F: Collection[F], N: Numeric[A]): A = {
    F.foldLeft(l, N.zero)((a, b) => N.plus(a, b))
  }

  def mean[F[_], A](s: F[A])(implicit f: Collection[F], N: Fractional[A]): A = {
    N.div(sum(s), N.fromInt(f.size(s)))
  }
}
