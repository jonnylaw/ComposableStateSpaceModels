package com.github.jonnylaw.model

import breeze.linalg.DenseVector
import breeze.numerics.{exp, log}
import breeze.stats.distributions.{Gaussian, Uniform, Exponential, Rand}
import com.github.jonnylaw.model.POMP._
import com.github.jonnylaw.model.Utilities._
import com.github.jonnylaw.model.DataTypes._
import com.github.jonnylaw.model.State._
import com.github.jonnylaw.model.ParticleFilter._
import breeze.linalg.linspace
import breeze.stats.distributions.Rand._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.File
import akka.stream.scaladsl._
import akka.util.ByteString
import Stream._
import com.github.jonnylaw.model.ParticleFilter._


object SimData {
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
    stepFun: (State, TimeIncrement) => Rand[State]): Stream[Sde] = {

    val deltat: TimeIncrement = Math.pow(10, -precision)

    // define a recursive stream from t0 to t = t0 + totalIncrement stepping by 10e-precision
    lazy val stream: Stream[Sde] = (Stream.cons(Sde(t0, x0),
      stream map (x => Sde(x.time + deltat, stepFun(x.state, deltat).draw)))).
      takeWhile (s => s.time <= t0 + totalIncrement)

    stream
  }

  /**
    * Simulates a diffusion process at any specified times
    * @param x0 the initial state
    * @param times a list of times at which to simulate the diffusion process
    * @param stepFun the transition kernel of a diffusion process which can be simulated from
    * @return a vector of Sde with values and times
    */
  def simSdeIrregular(
    x0: State,
    times: List[Time],
    stepFun: (State, TimeIncrement) => Rand[State]): Vector[Sde] = {

    val t0 = times.head

    times.tail.foldLeft(Vector(Sde(t0, x0)))((a, t1) => {
      val dt = t1 - a.head.time
      val x1 = stepFun(a.head.state, dt).draw

      Sde(t1, x1) +: a
    }).reverse
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
    mod: Model,
    precision: Int): Vector[Data] = {

    // generate an SDE Stream
    val stateSpace = simSdeStream(mod.x0.draw, start, end - start, precision, mod.stepFunction)

    // Calculate the upper bound of the stream
    val upperBound = stateSpace.map(s => mod.f(s.state, s.time)).
      map(exp(_)).max

    def loop(lastEvent: Time, eventTimes: Vector[Data]): Vector[Data] = {
      // sample from an exponential distribution with the upper bound as the parameter
      val t1 = lastEvent + Exponential(upperBound).draw

      if (t1 > end) {
        eventTimes
      } else {
        // drop the elements we don't need from the stream, then calculate the hazard near that time
        val statet1 = stateSpace.takeWhile(s => s.time <= t1) 
        val hazardt1 = statet1.map(s => mod.f(s.state, s.time)).last

        val stateEnd = statet1.last.state
        val gamma = mod.f(stateEnd, t1)
        val eta = mod.link(gamma)

        if (Uniform(0,1).draw <= exp(hazardt1)/upperBound) {
          loop(t1, Data(t1, true, Some(eta), Some(gamma), Some(statet1.last.state)) +: eventTimes)
         } else {
          loop(t1, eventTimes)
        }
      }
    }
    loop(start, stateSpace.map{ s => {
      val gamma = mod.f(s.state, s.time)
      val eta = mod.link(gamma)
      Data(s.time, false, Some(eta), Some(gamma), Some(s.state)) }}.toVector
    )
  }

  /**
    * Generates a vector of event times from the Log-Gaussian Cox-Process
    * by thinning an exponential process, returns the value of the state space at the event times only
    * @param start the start time of the process
    * @param end the end time of the process
    * @param mod the model used to generate the events. In a composition, LogGaussiancox must be the
    * left-hand model
    * @param precision the size of the discretized grid to simulate the latent state on, 10e-precision
    * @return a vector of Data at representing only the times which events happened
    */
  def simLGCPEvents(
    start: Time,
    end: Time,
    mod: Model,
    precision: Int): Vector[Data] = {

    // generate an SDE Stream
    val stateSpace = simSdeStream(mod.x0.draw, start, end - start, precision, mod.stepFunction)

    // Calculate the upper bound of the stream
    val upperBound = stateSpace.map(s => mod.f(s.state, s.time)).
      map(exp(_)).max

    def loop(lastEvent: Time, eventTimes: Vector[Data]): Vector[Data] = {
      // sample from an exponential distribution with the upper bound as the parameter
      val t1 = lastEvent + Exponential(upperBound).draw

      if (t1 > end) {
        eventTimes.reverse
      } else {
        // drop the elements we don't need from the stream, then calculate the hazard near that time
        val statet1 = stateSpace.takeWhile(s => s.time <= t1) 
        val hazardt1 = statet1.map(s => mod.f(s.state, s.time)).last

        val stateEnd = statet1.last.state
        val gamma = mod.f(stateEnd, t1)
        val eta = mod.link(gamma)

        if (Uniform(0,1).draw <= exp(hazardt1)/upperBound) {
          loop(t1, Data(t1, true, Some(eta), Some(gamma), Some(stateEnd)) +: eventTimes)
         } else {
          loop(t1, eventTimes)
        }
      }
    }
    loop(start, Vector())
  }


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
  case class ForecastOut(t: Time, obs: Observation, obsIntervals: CredibleInterval, eta: Double, etaIntervals: CredibleInterval,
    state: State, stateIntervals: IndexedSeq[CredibleInterval]) {

    override def toString = s"$t, $obs, ${obsIntervals.toString}, $eta, ${etaIntervals.toString}, ${state.flatten.mkString(", ")}, ${stateIntervals.mkString(", ")}"
  }

  /**
    * Forecast Data given the most recently estimated state
    * Return a vector of Data and credible intervals
    * @param x0 a vector containing realisations from the state, 
    * this can be from a distribution of Rand[State], by calling .sample(1000) 
    * or output from a particle filter
    * @param times a sequence of times to forecast at
    * @param mod an exponential family model to simulate forward from
    * @return a sequence of ForecastOut
    */
  def forecastData(x0: Vector[State], times: Seq[Time], mod: Model): Seq[ForecastOut] = {
    val samples = x0 map (simDataInit(_, times, mod))

    for {
      sample <- samples.transpose
      t = sample.head.t
      state = sample.map(_.sdeState.get)
      stateIntervals = getAllCredibleIntervals(state, 0.995)
      statemean = meanState(state)
      etas = state map (x => mod.link(mod.f(x, t)))
      meanEta = breeze.stats.mean(etas.map(_.head))
      etaIntervals = getOrderStatistic(etas.map(_.head), 0.995)
      obs = breeze.stats.mean(etas map (mod.observation(_).draw))
      obsIntervals = getOrderStatistic(etas map (mod.observation(_).draw), 0.995)
    } yield ForecastOut(t, obs, obsIntervals, meanEta, etaIntervals, statemean, stateIntervals)
  }


  /**
    * Simulate a single step of an exponential family model
    * @param x0 the initial state at the time of the last observation
    * @param t0 the time of the last observation
    * @param deltat the time increment to the next observation
    * @param mod a composed model, not the LogGaussiancox model
    * @return a Data element with the next latent state and observation
    */
  def simStep(
    x0: State,
    t0: Time,
    deltat: TimeIncrement,
    mod: Model): Data = {

    val x1 = mod.stepFunction(x0, deltat).draw
    val gamma = mod.f(x1, t0)
    val eta = mod.link(gamma)
    val y1 = mod.observation(eta).draw
    Data(t0, y1, Some(eta), Some(gamma), Some(x1))
  }

  /**
    * Given an initial state, simulate from a model
    * @param x0 the initial state
    * @param times a list of times to simulate the data
    * @param mod an exponential family model
    * @return a sequence of Data
    */
  def simDataInit(x0: State, times: Seq[Time], mod: Model): Seq[Data] = {
    val d0 = simStep(x0, times.head, 0, mod)

    val data = times.tail.scanLeft(d0) { (d0, t) =>
      val deltat = t - d0.t
      val x0 = d0.sdeState.get
      simStep(x0, t, deltat, mod)
    }

    data.reverse
  }

  /**
    * Simulate data from a list of times, allowing for irregular observations
    * @param times a list of times to simulate observations at
    * @param mod an exponential family model
    * @return a sequence of Data
    */
  def simData(times: Seq[Time], mod: Model): Seq[Data] = {

    val x0 = mod.x0.draw
    simDataInit(x0, times, mod)
  }

  /**
    * Simulate a step using Rand (a monad representing a distribution which can be sampled from)
    * @param x0 the initial state at the time of the previous observation
    * @param t the time of the previous observation
    * @param deltat the time increment to the time of the next observation
    * @param mod an exponential family model to simulate from
    * @return a Rand monad containing data, which can be sampled from to give a realisation from a model
    */
  def simStepRand(x0: State, t: Time, deltat: TimeIncrement, mod: Model): Rand[Data] = {
    for {
      x1 <- mod.stepFunction(x0, deltat)
      gamma = mod.f(x1, t)
      eta = mod.link(gamma)
      y1 <- mod.observation(eta)
    } yield Data(t, y1, Some(eta), Some(gamma), Some(x1))
  }

  /**
    * Simulate a vector of Data using Rand (a monad representing a distribution which can be sampled from)
    * @param times a list of times to observe the process at
    * @param mod an exponential family model
    * @return a Rand monad containing a vector of data
    */
  def simDataRand(times: Seq[Time], mod: Model): Rand[Seq[Data]] = {
    val x0 = mod.x0.draw
    val init = simStepRand(x0, times.head, 0, mod)

    val data = times.tail.scanLeft(init) { (d, t) => 
      for {
        d0 <- d
        deltat = t - d0.t
        x0 = d0.sdeState.get
        d1 <- simStepRand(x0, t, deltat, mod)
      } yield d1
    }

    sequence(data.reverse)
  }

  /**
    * Simulate data as an Akka Stream, with regular time intervals
    * @param mod The model to simulate from, can be composed or single
    * @param precision Used to determine the step length, dt = 10^-precision
    * @param t0 the starting time of the process
    * @return a source of an Akka Stream
    */
  def simStream(mod: Model, precision: Int, t0: Time): Source[Data, Any] = {
    val dt = math.pow(10, -precision)

    val x0 = mod.x0.draw
    val initialObservation = simStep(x0, t0, 0, mod)

    Source.unfold(initialObservation){d => Some((simStep(d.sdeState.get, d.t + dt, dt, mod), d)) }
  }
}
