package com.github.jonnylaw.model

import breeze.stats.distributions.{Rand, Uniform, Multinomial}
import breeze.numerics.{exp, log}
import breeze.linalg.DenseVector

import cats._
import cats.data.Reader
import cats.implicits._

import fs2._

import scala.collection.parallel.immutable.ParVector
import scala.language.higherKinds
import scala.reflect.ClassTag

/**
  * A typeclass representing a Collection with a few additional 
  * features required for implementing the particle filter
  */
trait Collection[F[_]] extends Foldable[F] with MonoidK[F] with Monad[F] {
  def scanLeft[A, B, C](fa: F[A], z: B)(f: (B, A) => B): F[B]
  def get[A](fa: F[A])(i: Int): A
  def indices[A](fa: F[A]): F[Int]

  /**
    * Expand a into a range of Numerics
    */
  def range[A](from: A, to: A, by: A)(implicit N: Numeric[A], f: MonoidK[F], app: Applicative[F]) = {
    def loop(acc: F[A], current: A): F[A] = {
      if (N.gteq(current, to)) {
        acc
      } else {
        loop(f.combineK(acc, app.pure(N.plus(current, by))), N.plus(current, by))
      }
    }
    loop(app.pure(from), from)
  }

  def indexWhere[A](fa: F[A])(cond: A => Boolean): Int

  def max[A: Ordering](fa: F[A]): A

  /**
    * Fund the sum of numeric values in a foldable
    */
  def sum[A](l: F[A])(implicit F: Foldable[F], N: Numeric[A]): A = {
    F.foldLeft(l, N.zero)((a, b) => N.plus(a, b))
  }

  /**
    * Generic Mean Function
    */
  def mean[A](s: F[A])(implicit f: Foldable[F], N: Fractional[A]): A = {
    N.div(sum(s), N.fromInt(s.size.toInt))
  }

  def fill[A](n: Int)(a: A): F[A]
  
  def toArray[A: ClassTag](fa: F[A]): Array[A]

  def toVector[A](fa: F[A]): Vector[A]

  def unzip[A, B](fa: F[(A, B)]): (F[A], F[B])

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
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

trait ParticleFilter[F[_]] {
  implicit val f: Collection[F]

  import ParticleFilter._

  val mod: Model

  def initialiseState(particles: Int, t0: Time): PfState[F] = {
    val state = f.fill(particles)(mod.sde.initialState.draw)
    PfState[F](t0, None, state, 0.0, particles)
  }

  def resample: Resample[F, State]

  def stepFilter(s: PfState[F], y: Data): PfState[F] = {
    val dt = y.t - s.t
    val x1 = s.particles map (x => mod.sde.stepFunction(dt)(x).draw)
    val w = x1 map (x => mod.dataLikelihood(mod.f(x, y.t), y.observation))
    val max = f.max(w)
    val w1 = f.map(w)(a => exp(a - max))
    val ll = s.ll + max + log(f.mean(w1))
    val resampledX = resample(x1, w1)
    val ess = ParticleFilter.effectiveSampleSize(w1)
    
    PfState(y.t, Some(y.observation), resampledX, ll, ess)
  }

  /**
    * Filter a collection of data and return an estimate of the loglikelihood
    */
  def llFilter(t0: Time, n: Int)(data: Vector[Data]) = {
    val initState = initialiseState(n, t0)
    data.foldLeft(initState)(stepFilter).ll
  }

  /**
    * Run a filter over a vector of data and return a vector of PfState
    * Containing the raw particles and associated weights at each time step
    */
  def accFilter(data: Vector[Data], t0: Time)(particles: Int) = {
    val initState = initialiseState(particles, t0)

    data.scanLeft(initState)(stepFilter).
      drop(1)
  }

  /**
    * Filter the data, but get a vector containing the mean gamma, gamma intervals, mean state, 
    * and credible intervals of the state
    */
  def filterWithIntervals(data: Vector[Data], t0: Time)(particles: Int) = {
    accFilter(data, t0)(particles).
      map(s => PfState(s.t, s.observation, f.toVector(s.particles), s.ll, s.ess)).
      map(getIntervals(mod))
  }

  /**
    * Run a filter over a stream of data
    */
  def filter(t0: Time)(particles: Int): Pipe[Task, Data, PfState[F]] = {
    val initState = initialiseState(particles, t0)

    pipe.scan(initState)(stepFilter)
  }
}

case class FilterSequential(mod: Model, resample: Resample[Vector, State]) extends ParticleFilter[Vector] {
  override val f = implicitly[Collection[Vector]]
}

/**
  * Particle filter which uses ParVector to represent the collection of particles
  * @param mod the model to use for filtering
  * @param resample the resampling scheme
  */
case class FilterParallel(mod: Model, resample: Resample[ParVector, State]) extends ParticleFilter[ParVector] {
  override val f = implicitly[Collection[ParVector]]
}

/**
  * Credible intervals from a set of samples in a distribution
  * @param lower the lower interval
  * @param upper the upper interval
  */
case class CredibleInterval(lower: Double, upper: Double) {
  override def toString = lower + ", " + upper
}

case class FilterLgcp(mod: Model, resample: Resample[Vector, State], precision: Int) extends ParticleFilter[Vector] {
  override val f = implicitly[Collection[Vector]]

  def calcWeight(x: State, dt: TimeIncrement, t: Time): (State, Double, Double) = {
    // calculate the amount of realisations of the state we need
    val n = Math.floor((t + dt) / Math.pow(10, -precision)).toInt

    // build a stream capable of materializing all the values of the state
    val x1 = mod.sde.simStreamInit[Nothing](t, x, Math.pow(10, -precision)).take(n)

    // get the value in the last position of the stream
    val lastState = x1.toVector.last.state

    // transform the state
    val gamma = mod.f(lastState, t + dt)

    // calculate the cumulative hazard
    val cumulativeHazard = x1.map((a: StateSpace) => mod.f(a.state, a.time)).
      map(x => exp(x) * Math.pow(10, -precision)).
      fold(0.0)(_+_).
      toVector.
      head

    (lastState, gamma, cumulativeHazard)
  }

  override def stepFilter(s: PfState[Vector], y: Data): PfState[Vector] = {
    val dt = y.t - s.t
    val state = s.particles.map(x => calcWeight(x, dt, y.t))
    val w = state.map(a => (a._2, a._3)).map { case (g,l) => g - l }
    val max = w.max
    val w1 = w map (a => exp(a - max))
    val ll = s.ll + max + log(breeze.stats.mean(w1))

    val resampledX = resample(state.map(_._1), w)
    val ess = ParticleFilter.effectiveSampleSize(w1)

    PfState[Vector](y.t, Some(y.observation), resampledX, ll, ess)
  }
}

case class Filter[F[_]: Collection](mod: Model, resample: Resample[F, State]) extends ParticleFilter[F] {
 override val f = implicitly[Collection[F]]
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
  def filter[F[_]: Collection](
    resample: Resample[F, State], 
    t0: Time, 
    n: Int
  ): Reader[Model, Pipe[Task, Data, PfState[F]]] = Reader { (mod: Model) =>

    Filter[F](mod, resample).filter(t0)(n)
  }

  def filterWithIntervals[F[_]: Collection](
    data: Vector[Data],
    resample: Resample[F, State], 
    t0: Time, 
    n: Int
  ) = Reader { (mod: Model) =>

    Filter[F](mod, resample).filterWithIntervals(data, t0)(n)
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
  def likelihood(data: Vector[Data], n: Int, resample: Resample[Vector, State]) = Reader { (mod: Model) => 
    FilterSequential(mod, resample).
      llFilter(data.map((d: Data) => d.t).min, n)(data.sortBy((d: Data) => d.t))
  }

  def parLikelihood(data: Vector[Data], n: Int) = Reader { (mod: Model) => 
    FilterParallel(mod, systematicResampling[ParVector, State]).
      llFilter(data.map((d: Data) => d.t).min, n)(data.sortBy((d: Data) => d.t))
  }

  /**
    * Given a state of the particle filter, advance the state and calculate the mean 
    * of the state, gamma and forecast observation
    * @param s the state of the particle filter
    * @param mod the model used to predict the observations
    * @param t the time of the prediction
    * @return ForecastOut, a summary containing the mean of the state, gamma and observation
    */
  def getMeanForecast[F[_]](s: PfState[F], mod: Model, t: Time)(implicit f: Collection[F]): ForecastOut = {
    val state: Vector[State] = f.toVector(s.particles)

    val dt = t - s.t
    val x1 = state map (mod.sde.stepFunction(dt)(_).draw)
    val gamma = x1 map (x => mod.link(mod.f(x, t)))

    val stateIntervals = getallCredibleIntervals(x1.toVector, 0.995)
    val statemean = meanState(x1.toVector)
    val meanGamma = breeze.stats.mean(gamma)
    val gammaIntervals = getOrderStatistic(gamma.toVector, 0.995)
    val obs = breeze.stats.mean(gamma.map(mod.observation(_).draw))
    val obsIntervals = getOrderStatistic(gamma.map(mod.observation(_).draw).toVector, 0.995)

    ForecastOut(t, obs, obsIntervals, meanGamma, gammaIntervals, statemean, stateIntervals)
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
    val normalisedWeights = normalise(weights)
    math.floor(1 / f.sum(normalisedWeights.map(w => w * w))).toInt
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
    * Given a vector of doubles, returns a normalised vector with probabilities summing to one
    * @param prob a vector of unnormalised probabilities
    * @return a vector of normalised probabilities
    */
  def normalise[F[_]](prob: F[Double])(implicit f: Collection[F]): F[Double] = {
    val total = f.sum(prob)
    f.map(prob)(x => x/total)
  }

  /**
    * Generic cumulative sum
    */
  def cumSum[F[_], A](l: F[A])(implicit f: Collection[F], N: Numeric[A]): F[A] = {
    f.scanLeft(l, N.zero)((a, b) => N.plus(a, b))
  }

  /**
    * Calculate the empirical cumulative distribution function for a collection of weights
    */
  def ecdf[F[_]](w: F[Double])(implicit f: Collection[F]) = {
    val normalised = f.map(w)(weight => weight / f.sum(w))
    cumSum(normalised)
  }

 /**
    * Multinomial Resampling, sample from a categorical distribution with probabilities
    * equal to the particle weights 
    */
  def multinomialResampling[F[_], A](particles: F[A], weights: F[LogLikelihood])(implicit f: Collection[F]): F[A] = {
    val indices = f.fill(f.size(particles).toInt)(Multinomial(DenseVector(f.toArray(weights))).draw)
    f.map(indices)(i => f.get(particles)(i))
  }

  /**
    * Stratified resampling
    * Sample n ORDERED uniform random numbers (one for each particle) using a linear transformation of a U(0,1) RV
    */
  def stratifiedResampling[F[_], A](s: F[A], w: F[Double])(implicit f: Collection[F]): F[A] = {
    val n = w.size
    val empiricalDist = ecdf(w)

    f.range(1, n, 1).map(i => {
      val k = (i - 1 + scala.util.Random.nextDouble) / n
      f.indexWhere(empiricalDist)(e => e > k)
    }).
    map(i => f.get(s)(i-1))
  }

  /**
    * Generic systematic Resampling
    */
  def systematicResampling[F[_], A](s: F[A], w: F[Double])(implicit f: Collection[F]) = {
    val u = scala.util.Random.nextDouble // generate a uniform random number 
    val n = f.size(s)
    // val k = f.range(1, n, 1).map(i => (i - 1 + u) / n)

    val empiricalDist = ecdf(w)

    f.range(1, n, 1).
      map(i => f.indexWhere(empiricalDist)(e => e > ((i - 1 + u) / n))).
      map(i => f.get(s)(i-1))
  }

  /**
    * Residual Resampling
    * Select particles in proportion to their weights, ie particle xi appears ki = n * wi times
    * Resample m (= n - total allocated particles) particles according to w = n * wi - ki using other resampling technique
    */
  def residualResampling[A](particles: Vector[A], weights: Vector[Double]): Vector[A] = {
    val n = weights.length
    val normalisedWeights = normalise(weights)
    val ki = normalisedWeights.
      map (w => math.floor(w * n).toInt)

    val indices = ki.zipWithIndex.
      map { case (n, i) => Vector.fill(n)(i) }.
      flatten
    val x = indices map { particles(_) }
    val m = n - indices.length
    val residualWeights = normalisedWeights.zip(ki) map { case (w, k) => n * w - k }

    val i = multinomialResampling(Vector.range(1, m), residualWeights)
    x ++ (i map { particles(_) })
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
}
