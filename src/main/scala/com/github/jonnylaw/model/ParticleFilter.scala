package com.github.jonnylaw.model

import akka.stream.scaladsl._
import akka.stream._
import akka.NotUsed

import breeze.stats.distributions.{Rand, Multinomial}
import breeze.numerics.{exp, log}
import breeze.linalg.DenseVector

import cats._
import cats.data.{Reader, Kleisli}
import cats.implicits._
import cats.instances._

import GraphDSL.Implicits._

import scala.collection.parallel.immutable.ParVector
import scala.concurrent._
import scala.language.higherKinds

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
case class PfState(
  t: Time,
  observation: Option[Observation],
  particles: Vector[State],
  ll: LogLikelihood, 
  ess: Int)

case class PfStateInterpolate(
  t: Time,
  observation: Option[Observation],
  particles: Vector[List[State]],
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
  * @param params a the parameters
  * @param state the state particle cloud
  * @param ll the log likelihood
  */
case class ParamFilterState(
  t: Time,
  observation: Option[Observation],
  params: Parameters,
  state: Vector[State],
  ll: LogLikelihood)

trait ParticleFilter {
  import ParticleFilter._

  val mod: Model

  def initialiseState(particles: Int, t0: Time): PfState = {
    val state = Vector.fill(particles)(mod.sde.initialState.draw)
    PfState(t0, None, state, 0.0, particles)
  }

  def resample: Resample[State]

  /**
    * A single step of the particle filter; upon the absence of an observation
    * the state is simulated forward without resampling
    */
  def stepFilter(s: PfState, y: Data): PfState = {
    val dt = y.t - s.t
    val x1 = s.particles map (x => mod.sde.stepFunction(dt)(x).draw)

    y.observation match {
      case None => PfState(y.t, None, x1, s.ll, s.ess)
      case Some(obs) => 
      val w = x1 map (x => mod.dataLikelihood(mod.f(x, y.t), obs))
      val max = w.max
      val w1 = w map (a => exp(a - max))
      val resampledX = resample(x1, w1)
      val ll = s.ll + max + log(ParticleFilter.mean(w1))
      val ess = ParticleFilter.effectiveSampleSize(w1)

      PfState(y.t, Some(obs), resampledX, ll, ess)
    }
  }

  /**
    * Filter a collection of data and return an estimate of the loglikelihood
    */
  def llFilter(data: Vector[Data])(n: Int): LogLikelihood = {
    val initState = initialiseState(n, data.minBy(_.t).t)
    data.foldLeft(initState)(stepFilter).ll
  }

  /**
    * A particle filter to be ran over observed data to return the log-likelihood and a proposed value
    * of the path by sampling from the distribution of the paths
    * @param data the initial time of the data
    * @param particles the number of particles to use in the particle approximation to the filtering distribution
    * @return (LogLikelihood, Vector[State]) The log likelihood and a sample from the posterior of the filtering distribution
    */
  def filter(data: Vector[Data])(particles: Int): (LogLikelihood, Vector[StateSpace]) = {
    val init = initialiseState(particles, data.minBy(_.t).t)
    val states = data.scanLeft(init)(stepFilter)
    val ll = states.last.ll
    
    (ll, states.map(x => StateSpace(x.t, Resampling.sampleOne(x.particles))))
  }

  /**
    * Run a filter over a stream of data
    */
  def filterStream(t0: Time)(particles: Int): Flow[Data, PfState, NotUsed] = {
    val init = initialiseState(particles, t0)
    Flow[Data].scan(init)(stepFilter)
  }
}

case class FilterLgcp(
  mod: Model,
  resample: Resample[State], 
  precision: Int) extends ParticleFilter {

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

  override def stepFilter(s: PfState, y: Data): PfState = {
    val dt = y.t - s.t
    val state = if (dt == 0) {
      s.particles map (x => (x, mod.f(x, y.t), mod.f(x, y.t)))
    } else {
      s.particles.map(x => calcWeight(x, dt, y.t))
    }
    val w = state.map(a => (a._2, a._3)).map { case (g,l) => g - l }
    val max = w.max
    val w1 = w map (a => exp(a - max))
    val ll = s.ll + max + log(ParticleFilter.mean(w1))

    val ess = ParticleFilter.effectiveSampleSize(w1)
    val resampledX = resample(state.map(_._1), w1)

    PfState(y.t, y.observation, resampledX, ll, ess)
  }
}

/**
  * A particle filter which can represent particle clouds as a Collection, Collection is a typeclass
  * which can currently has concrete methods for Vector and ParVector 
  * @param mod
  */
case class Filter(mod: Model, resample: Resample[State]) extends ParticleFilter

/**
  * Provide a value of the state to start the particle filter at, this is used to initialise the 
  * state instead of a draw from the initial state
  */
case class FilterInit(mod: Model, resample: Resample[State], initState: State) extends ParticleFilter {
  override def initialiseState(particles: Int, t0: Time): PfState = {
    val state = Vector.fill(particles)(initState)
    PfState(t0, None, state, 0.0, particles)
  }
}

case class FilterInterpolate(mod: Model, resample: Resample[List[State]]) {
  /**
    * A single step of the particle filter, upon the absence of an observation
    * the state is simulated forward and the entire path is resampled when there is an accompanying observation 
    */
  def stepInterpolate(s: PfStateInterpolate, y: Data): PfStateInterpolate = {
    val dt = y.t - s.t
    // advance each particle and append the ancestors to the tail
    val x1 = s.particles map (x => mod.sde.stepFunction(dt)(x.head).draw :: x)

    y.observation match {
      case None => PfStateInterpolate(y.t, None, x1, s.ll, s.ess)
      case Some(obs) => 
        val w = x1 map (x => mod.dataLikelihood(mod.f(x.head, y.t), obs))
        val max = w.max
        val w1 = w map (a => exp(a - max))
        val resampledX = resample(x1, w1.toVector)
        val ll = s.ll + max + log(ParticleFilter.mean(w1))
        val ess = ParticleFilter.effectiveSampleSize(w1)

        PfStateInterpolate(y.t, Some(obs), resampledX, ll, ess)
    }
  }

  def filterInterpolate(t0: Time)(particles: Int): Flow[Data, PfStateInterpolate, NotUsed] = {
    // initialise the state as a vector of lists, with each list containing the first simulation of a path, x0
    val x0 = mod.sde.initialState.sample(particles).toVector map { _ :: Nil }
    val initState = PfStateInterpolate(t0, None, x0, 0.0, 0)
    
    Flow[Data].scan(initState)(stepInterpolate).
      map(s => s.copy(particles = s.particles.reverse))
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
  def filter(resample: Resample[State], t0: Time, n: Int) = Reader { (mod: Model) =>
    Filter(mod, resample).filterStream(t0)(n)
  }

  /**
    * Filter from a value of the state
    */
  def filterInit(resample: Resample[State], t0: Time, n: Int, initState: State) = Reader { (mod: Model) =>
    FilterInit(mod, resample, initState).filterStream(t0)(n)
  }

  /**
    * Function which takes a sample from the joint posterior
    * then runs and combines many particle filters
    * @param post the joint posterior distribution of the parameters and the state
    * @param samples the number of samples to take from the joint posterior, this determines the number of particle filters
    * @param t0 the initial starting point of the particle filter
    * @param n the number of particles in each of the particle filters
    * @param resample the resampling scheme
    * @param mod an unparameterised model to use for the online filter
    * @retrun an Akka stream Flow which can be used to run many particle filters online
    */
  def filterOnline(
    post: Rand[(Parameters, State)],
    samples: Int,
    t0: Time,
    n: Int,
    resample: Resample[State],
    mod: UnparamModel
  ) = Flow.fromGraph(GraphDSL.create() {
      implicit builder: GraphDSL.Builder[NotUsed] =>
      
    val values: IndexedSeq[(Parameters, State)] = post.sample(samples)
    
    val bcast = builder.add(Broadcast[Data](samples))
    val merge = builder.add(Merge[PfState](samples))
    val reduce = builder.add(Flow[PfState].reduce((a, b) =>
      PfState(a.t, a.observation, a.particles ++ b.particles, b.ll, b.ess + a.ess)))

    val summarise = builder.add(Flow[PfState].map(ParticleFilter.getIntervals(mod, values.head._1).run))

    val filterFlow = (s: (Parameters, State)) => {
      val pf = filterInit(resample, t0, n, s._2).lift[Error].compose(mod)
      pf(s._1)
    }

    bcast ~> filterFlow(values.head).right.get ~> merge ~> reduce ~> summarise
    bcast ~> filterFlow(values(1)).right.get   ~> merge

    FlowShape(bcast.in, summarise.out)
  })

  /**
    * 
    */
  def interpolate(resample: Resample[List[State]], t0: Time, particles: Int) = Reader { (mod: Model) =>
    FilterInterpolate(mod, resample).filterInterpolate(t0)(particles)
  }

  /**
    * Return the likelihood and a sample from the state path as a tuple
    * @param data a vector containing observations
    * @param resample a method of resampling in the particle filter
    * @param n the number of particles to use in the particle filter
    * @param model an unparameterised model
    */
  def filterLlState(data: Vector[Data], resample: Resample[State], n: Int) = Reader { 
    (mod: Model) => Filter(mod, resample).filter(data)(n)
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
  def likelihood(data: Vector[Data], resample: Resample[State], n: Int): Reader[Model, LogLikelihood] = Reader { 
    (model: Model) => Filter(model, resample).llFilter(data.sortBy((d: Data) => d.t))(n)
  }

  /**
    * Given an initial set of particles, representing an approximation of the posterior
    * distribution of the filtering state at time t0, simulate the particles forward
    * calculating the predicted observation distribution and intervals
    */
  def getForecast(
    s: PfState, 
    mod: Model, 
    t: Time): Vector[ObservationWithState] = {
    val dt = t - s.t

    s.particles map { state =>
      val x1 = mod.sde.stepFunction(dt)(state).draw
      val gamma = mod.f(x1, t)
      val eta = mod.link(gamma)
      val obs = mod.observation(gamma)

      ObservationWithState(t, Some(obs.draw), eta, gamma, x1)
    }
  }

  /**
    * Given a state of the particle filter, advance the state and calculate the mean 
    * of the state, gamma and forecast observation
    * @param s the state of the particle filter
    * @param mod the model used to predict the observations
    * @param t the time of the prediction
    * @return ForecastOut, a summary containing the mean of the state, gamma and observation
    */
  def getMeanForecast(s: PfState, mod: Model, t: Time, interval: Double): ForecastOut = {

    val forecast = getForecast(s, mod, t)

    val stateIntervals = getallCredibleIntervals(forecast.map(_.sdeState).toVector, interval)
    val statemean = meanState(forecast.map(_.sdeState).toVector)
    val meanEta = breeze.stats.mean(forecast.map(_.eta))
    val etaIntervals = getOrderStatistic(forecast.map(_.eta).toVector, interval)
    val obs = forecast.map(x => mod.observation(x.gamma).draw)
    val meanObs = breeze.stats.mean(obs)
    val obsIntervals = getOrderStatistic(obs, interval)

    ForecastOut(t, meanObs, obsIntervals, 
      meanEta, etaIntervals, statemean, stateIntervals)
  }

  /**
    * Transforms PfState into PfOut, including eta, eta intervals and state intervals
    */
  def getIntervals(model: UnparamModel, params: Parameters) = Reader { (s: PfState) =>
    model(params) map { mod =>
      val state = s.particles
      val stateMean = meanState(state)
      val stateIntervals = getallCredibleIntervals(state, 0.975)
      val etas = state map (x => mod.link(mod.f(x, s.t)))
      val meanEta = mod.link(mod.f(stateMean, s.t))
      val etaIntervals = getOrderStatistic(etas, 0.975)

      PfOut(s.t, s.observation, meanEta, etaIntervals, stateMean, stateIntervals)
    }
  }

  /**
    * Calculate the effective sample size of a particle cloud, from the un-normalised weights
    * by first normalising the weights, then calculating the reciprocal of the sum of the squared weights
    * @param weights the unnormalised weights
    */
  def effectiveSampleSize(weights: Seq[Double]): Int = {
    val normalisedWeights = Resampling.normalise(weights)
    math.floor(1 / normalisedWeights.map(w => w * w).sum).toInt
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

  def mean[A](s: Seq[A])(implicit N: Fractional[A]): A = {
    N.div(s.sum, N.fromInt(s.size))
  }
}
