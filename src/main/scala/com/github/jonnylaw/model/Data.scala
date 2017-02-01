package com.github.jonnylaw.model

import breeze.stats.distributions.{Rand, Exponential, Process, MarkovChain, Uniform}
import fs2._
import breeze.numerics.{exp, sqrt}

/**
  * A single observation of a time series
  */
sealed trait Data {
  val t: Time
  val observation: Observation
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
  observation: Observation,
  eta: Eta,
  gamma: Gamma,
  sdeState: State) extends Data

/**
  * A single observation of a time series
  * @param t, the time of the observation
  * @param observation = pi(eta), the observation
  */
case class TimedObservation(t: Time, observation: Observation) extends Data

trait DataService {
  def observations[F[_]]: Stream[F, Data]
}

case class SimulatedData(model: Model) extends DataService {
  def observations[F[_]] = simRegular[F](0.1)

  /**
    * Simulate a single step from a model, return a distribution over the possible values
    * of the next step
    * @param deltat the time difference between the previous and next realisation of the process
    * @return a function from the previous datapoint to a Rand (Monadic distribution) representing 
    * the distribution of the next datapoint 
    */
  def simStep(deltat: TimeIncrement) = (d: ObservationWithState) => model match {
    case _: LogGaussianCox => throw new Exception("Can't simulate a step from the LGCP in this way")
    case _ =>
    for {
      x1 <- model.sde.stepFunction(deltat)(d.sdeState)
      gamma = model.f(x1, d.t + deltat)
      eta = model.link(gamma)
      y1 <- model.observation(eta)
    } yield ObservationWithState(d.t + deltat, y1, eta, gamma, x1)
  }

  /**
    * Simulate from a POMP model on an irregular grid, given an initial time and a stream of times 
    * at which simulate from the model
    * @param t0 the start time of the process
    * @return an Pipe transforming a Stream from Time to ObservationWithState 
    */
  def simPompModel(t0: Time): Pipe[Task, Time, ObservationWithState] = {
    val init = for {
      x0 <- model.sde.initialState
      gamma = model.f(x0, t0)
      eta = model.link(gamma)
      y <- model.observation(eta)
    } yield ObservationWithState(t0, y, eta, gamma, x0)

    pipe.scan(init.draw)((d0, t: Time) => simStep(t - d0.t)(d0).draw)
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
      y <- model.observation(eta)
    } yield ObservationWithState(0.0, y, eta, gamma, x0)

    MarkovChain(init.draw)(simStep(dt))
  }

  /**
    * Simulate from any model on a regular grid from t = 0 and return an Akka stream of realisations
    * @param dt the time increment between successive realisations of the POMP model
    * @return an Akka Stream containing a realisation of the process
    */
  def simRegular[F[_]](dt: TimeIncrement): Stream[F, ObservationWithState] = model match {
    case _: LogGaussianCox => throw new Exception("Not implemented")
    case _ =>
      fromProcess(simMarkov(dt))
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
    val upperBound = stateSpace.map(s => model.f(s.state, s.time)).
      map(exp(_)).max

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
        val gamma = model.f(stateEnd, t1)
        val eta = model.link(gamma)

        if (Uniform(0,1).draw <= exp(hazardt1)/upperBound) {
          loop(t1, ObservationWithState(t1, 1.0, eta, gamma, statet1.last.state) +: eventTimes)
         } else {
          loop(t1, eventTimes)
        }
      }
    }
    loop(start, stateSpace.map{ s => {
      val gamma = model.f(s.state, s.time)
      val eta = model.link(gamma)
      ObservationWithState(s.time, 0.0, eta, gamma, s.state) }}.toVector
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
}
