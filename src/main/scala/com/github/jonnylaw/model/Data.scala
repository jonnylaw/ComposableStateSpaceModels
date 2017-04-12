package com.github.jonnylaw.model

import akka.stream.scaladsl._
import akka.stream._
import akka.NotUsed
import akka.util.ByteString
import breeze.stats.distributions.{Rand, Exponential, Process, MarkovChain, Uniform}
import breeze.numerics.{exp, sqrt}
import java.nio.file._
import scala.concurrent.Future
import scala.language.higherKinds
import spray.json._
import DataProtocols._
import org.joda.time.DateTime

/**
  * A single observation of a time series
  */
sealed trait Data {
  val t: Time
  val observation: Option[Observation]
}

/**
  * A single observation of a time series, containing a realisation of the filtering state 
  * @param sdeState = x_t = p(x_t | x_t-1), the latent state
  * @param gamma = f(x_t), the latent state transformed by the linear transformation
  * @param eta = g(gamma), the latent state transformed by the linking-function
  * @param observation = pi(eta), the observation
  * @param t, the time of the observation
  */
case class ObservationWithState(
  t: Time,
  observation: Option[Observation],
  eta: Eta,
  gamma: Gamma,
  sdeState: State) extends Data

/**
  * A single observation of a time series
  * @param t, the time of the observation
  * @param observation = pi(eta), the observation
  */
case class TimedObservation(t: Time, observation: Option[Observation]) extends Data

case class TimestampObservation(timestamp: DateTime, t: Time, observation: Option[Observation]) extends Data

case class DecomposedModel(time: Time, observation: Option[Observation], eta: Eta, gamma: Gamma, state: List[Eta])

trait DataService[F] {
  def observations: Source[Data, F]
}

case class SimulateData(model: Model) extends DataService[NotUsed] {
  def observations: Source[Data, NotUsed] = simRegular(0.1)

  def simStep(deltat: TimeIncrement) = SimulateData.simStep(model)(deltat)

  /**
    * Simulate from a POMP model on an irregular grid, given an initial time and a stream of times
    * at which simulate from the model
    * @param t0 the start time of the process
    * @return an Akka Flow transforming a Stream from Time to ObservationWithState 
    */
  def simPompModel(t0: Time) = {
    val init = for {
      x0 <- model.sde.initialState
      gamma = model.f(x0, t0)
      eta = model.link(gamma)
      y <- model.observation(gamma)
    } yield ObservationWithState(t0, Some(y), eta, gamma, x0)

    Flow[Time].scan(init.draw)((d0, t: Time) => simStep(t - d0.t)(d0).draw)
  }

  /**
    * Simulate from a POMP model (not including the Log-Gaussian Cox-Process) 
    * on a regular grid from t = 0 using the MarkovChain from the breeze package
    * @param dt the time increment between sucessive realisations of the POMP model
    * @return a Process, representing a distribution which depends on previous draws
    */
  def simMarkov(dt: TimeIncrement): Process[ObservationWithState] = {
    
    val init = for {
      x0 <- model.sde.initialState
      gamma = model.f(x0, 0.0)
      eta = model.link(gamma)
      y <- model.observation(gamma)
    } yield ObservationWithState(0.0, Some(y), eta, gamma, x0)

    MarkovChain(init.draw)(simStep(dt))
  }

  /**
    * Simulate from any model on a regular grid from t = 0 and return an Akka stream of realisations
    * @param dt the time increment between successive realisations of the POMP model
    * @return an Akka Stream containing a realisation of the process
    */
  def simRegular(dt: TimeIncrement): Source[ObservationWithState, NotUsed] = {
    Source.fromIterator(() => simMarkov(dt).steps)
  }

  /**
    * Simulate the log-Gaussian Cox-Process using thinning
    * @param start the starting time of the process
    * @param the end time of the process
    * @param mod the model to simulate from. In a composition, the LogGaussianCox must be the left-hand model
    * @param precision an integer specifying the timestep between simulating the latent state, 10e-precision
    * @return a vector of Data specifying when events happened
    */
  def simLGCP(
    start: Time,
    end: Time,
    precision: Int): Vector[ObservationWithState] = {

    // generate an SDE Stream
    val stateSpace = SimulateData.simSdeStream(model.sde.initialState.draw,
      start, end - start, precision, model.sde.stepFunction)

    // Calculate the upper bound of the stream
    val upperBound = stateSpace.map(s => model.f(s.state, s.time)).map(exp(_)).max

    def loop(lastEvent: Time, eventTimes: Vector[ObservationWithState]): Vector[ObservationWithState] = {
      // sample from an exponential distribution with the upper bound as the parameter
      val t1 = lastEvent + Exponential(upperBound).draw

      if (t1 > end) {
        eventTimes
      } else {
        // drop the elements we don't need from the stream, then calculate the hazard near that time
        val statet1 = stateSpace.takeWhile(s => s.time <= t1) 
        val hazardt1 = statet1.map(s => model.f(s.state, s.time)).last

        val stateEnd = statet1.last.state
        val gamma = statet1 map (s => model.f(s.state, s.time))
        val eta = exp(gamma.last)

        if (Uniform(0,1).draw <= exp(hazardt1)/upperBound) {
          loop(t1, ObservationWithState(t1, Some(1.0), eta, gamma.last, stateEnd) +: eventTimes)
         } else {
          loop(t1, eventTimes)
        }
      }
    }
    loop(start, stateSpace.map{ s => {
      val gamma = model.f(s.state, s.time)
      val eta = exp(gamma)
      ObservationWithState(s.time, Some(0.0), eta, gamma, s.state) }}.toVector
    )
  }
}

object SimulateData {
  /**
    * Simulate a diffusion process as a stream
    * @param x0 the starting value of the stream
    * @param t0 the starting time of the stream
    * @param totalIncrement the ending time of the stream
    * @param precision the step size of the stream 10e(-precision)
    * @param stepFun the stepping function to use to generate the SDE Stream
    * @return a lazily evaluated stream of Sde
    */
  def simSdeStream(
    x0: State,
    t0: Time,
    totalIncrement: TimeIncrement,
    precision: Int,
    stepFunction: TimeIncrement => State => Rand[State]): scala.collection.immutable.Stream[StateSpace] = {

    val deltat: TimeIncrement = Math.pow(10, -precision)

    // define a recursive stream from t0 to t = t0 + totalIncrement stepping by 10e-precision
    scala.collection.immutable.Stream.
      iterate(StateSpace(t0, x0))(x =>
        StateSpace(x.time + deltat, stepFunction(deltat)(x.state).draw)).
      takeWhile(s => s.time <= t0 + totalIncrement)
  }

  /**
    * Simulate a single step from a model, return a distribution over the possible values
    * of the next step
    * @param model the model to simulate a step from
    * @param deltat the time difference between the previous and next realisation of the process
    * @return a function from the previous datapoint to a Rand (Monadic distribution) representing
    * the distribution of the next datapoint 
    */
  def simStep(model: Model)(deltat: TimeIncrement) = { (d: ObservationWithState) =>
    for {
      x1 <- model.sde.stepFunction(deltat)(d.sdeState)
      gamma = model.f(x1, d.t + deltat)
      eta = model.link(gamma)
      y1 <- model.observation(gamma)
    } yield ObservationWithState(d.t + deltat, Some(y1), eta, gamma, x1)
  }

  /**
    * Compute an empirical forecast, starting from a filtering distribution estimate
    * @param unparamModel a function from Parameters to Model
    * @param t the time to start the forecast
    * @param n the number of particles to use in the forecast
    * @param s the joint posterior of the parameters and state at time t, p(x, theta | y)
    */
  def forecast(unparamModel: Parameters => Model, t: Time, n: Int)(s: Rand[(Parameters, State)]) = {

    val init: IndexedSeq[(Parameters, ObservationWithState)] = s.sample(n) map { case (p, x) => {
      val gamma = unparamModel(p).f(x, t)
      val eta = unparamModel(p).link(gamma)
      (p, ObservationWithState(t, Some(0.0), eta, gamma, x))
    }}

    Flow[Time].
      scan(init)((d0, ts: Time) => {
        for {
          (p, x) <- d0
          x1 = simStep(unparamModel(p))(ts - x.t)(x).draw
        } yield (p, x1)}).
      drop(1) // drop the initial state
  }

  def summariseForecast(mod: Parameters => Model, interval: Double) = { (s: Vector[(Parameters, ObservationWithState)]) =>
    val forecast = s.map(_._2)

    val stateIntervals = ParticleFilter.getallCredibleIntervals(forecast.map(_.sdeState).toVector, interval)
    val statemean = ParticleFilter.meanState(forecast.map(_.sdeState).toVector)
    val meanEta = breeze.stats.mean(forecast.map(_.eta))
    val etaIntervals = ParticleFilter.getOrderStatistic(forecast.map(_.eta).toVector, interval)
    val obs = s.map { case (p, x) => mod(p).observation(x.gamma).draw }
    val obsIntervals = ParticleFilter.getOrderStatistic(obs, interval)

    ForecastOut(forecast.head.t, breeze.stats.mean(obs), obsIntervals,
      meanEta, etaIntervals, statemean, stateIntervals)
  }

  /**
    * Get the transformed state of the nth model
    * @state the state to transform from a composed model
    * @model a model component from the composed model which produced the state
    * @position the position of the model in the tree, indexed from zero
    */
  def getState(state: State, model: Model, position: Int)(t: Time): Eta = {
    val s = Tree.leaf(state.getNode(position))
    model.f(s, t)
  }
}

/**
  * Read a csv file in, where the first column corresponds to the Time, represented as a Double
  * and the second column represents the observation.
  * @param file a java.nio.file.Path to a file
  */
case class DataFromFile(file: String) extends DataService[Future[IOResult]] {
  def observations: Source[Data, Future[IOResult]] = {
    FileIO.fromPath(Paths.get(file)).
      via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
      map(_.utf8String).
      map(a => a.split(",")).
      map(d => TimedObservation(d(0).toDouble, if (d(1).isEmpty) { None } else { Some(d(1).toDouble) })) 
  }
}

/**
  * Read a JSON file
  */
case class DataFromJson(file: String) extends DataService[Future[IOResult]] {
  def observations: Source[Data, Future[IOResult]] = {
    FileIO.fromPath(Paths.get(file)).
      via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
      map(_.utf8String).
      map(_.parseJson.convertTo[TimedObservation])
  }
}
