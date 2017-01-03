// package com.github.jonnylaw.model

// import Utilities._
// import DataTypes._
// import State._
// import ParticleFilter._
// import Stream._
// import ParticleFilter._

// import breeze.linalg.linspace
// import breeze.stats.distributions.Rand._

// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.stream.scaladsl.Source
// import java.io.File
// import akka.stream.scaladsl._
// import akka.util.ByteString
// import breeze.linalg.DenseVector
// import breeze.numerics.{exp, log}
// import breeze.stats.distributions.{Gaussian, Uniform, Exponential, Rand}

// object SimData {
//   /**
//     * Simulates a diffusion process at any specified times
//     * @param x0 the initial state
//     * @param times a list of times at which to simulate the diffusion process
//     * @param stepFun the transition kernel of a diffusion process which can be simulated from
//     * @return a vector of Sde with values and times
//     */
//   def simSdeIrregular(
//     x0: State,
//     times: List[Time],
//     stepFun: (State, TimeIncrement) => Rand[State]): Vector[Sde] = {

//     val t0 = times.head

//     times.tail.foldLeft(Vector(Sde(t0, x0)))((a, t1) => {
//       val dt = t1 - a.head.time
//       val x1 = stepFun(a.head.state, dt).draw

//       Sde(t1, x1) +: a
//     }).reverse
//   }

//   /**
//     * Generates a vector of event times from the Log-Gaussian Cox-Process
//     * by thinning an exponential process, returns the value of the state space at the event times only
//     * @param start the start time of the process
//     * @param end the end time of the process
//     * @param mod the model used to generate the events. In a composition, LogGaussiancox must be the
//     * left-hand model
//     * @param precision the size of the discretized grid to simulate the latent state on, 10e-precision
//     * @return a vector of Data at representing only the times which events happened
//     */
//   def simLGCPEvents(
//     start: Time,
//     end: Time,
//     mod: Model,
//     precision: Int): Vector[Data] = {

//     // generate an SDE Stream
//     val stateSpace = Model.simSdeStream(mod.x0.draw, start, end - start, precision, mod.stepFunction)

//     // Calculate the upper bound of the stream
//     val upperBound = stateSpace.map(s => mod.f(s.state, s.time)).
//       map(exp(_)).max

//     def loop(lastEvent: Time, eventTimes: Vector[Data]): Vector[Data] = {
//       // sample from an exponential distribution with the upper bound as the parameter
//       val t1 = lastEvent + Exponential(upperBound).draw

//       if (t1 > end) {
//         eventTimes.reverse
//       } else {
//         // drop the elements we don't need from the stream, then calculate the hazard near that time
//         val statet1 = stateSpace.takeWhile(s => s.time <= t1) 
//         val hazardt1 = statet1.map(s => mod.f(s.state, s.time)).last

//         val stateEnd = statet1.last.state
//         val gamma = mod.f(stateEnd, t1)
//         val eta = mod.link(gamma)

//         if (Uniform(0,1).draw <= exp(hazardt1)/upperBound) {
//           loop(t1, Data(t1, true, Some(eta), Some(gamma), Some(stateEnd)) +: eventTimes)
//          } else {
//           loop(t1, eventTimes)
//         }
//       }
//     }
//     loop(start, Vector())
//   }

//   /**
//     * Forecast data
//     * @param t the time of the observation
//     * @param obs an observation of the process
//     * @param obsIntervals the upper and lower credible intervals of the observation
//     * @param eta the transformed latent state
//     * @param etaIntervals the credible intervals of the transformed latent state
//     * @param state the untransformed latent state
//     * @param stateIntervals the intervals of the latent state
//     */
//   case class ForecastOut(t: Time, obs: Observation, obsIntervals: CredibleInterval, eta: Double, etaIntervals: CredibleInterval,
//     state: State, stateIntervals: IndexedSeq[CredibleInterval]) {

//     override def toString = s"$t, $obs, ${obsIntervals.toString}, $eta, ${etaIntervals.toString}, ${state.flatten.mkString(", ")}, ${stateIntervals.mkString(", ")}"
//   }

//   // /**
//   //   * Forecast Data given the most recently estimated state
//   //   * Return a vector of Data and credible intervals
//   //   * @param x0 a vector containing realisations from the state, 
//   //   * this can be from a distribution of Rand[State], by calling .sample(1000) 
//   //   * or output from a particle filter
//   //   * @param times a sequence of times to forecast at
//   //   * @param mod an exponential family model to simulate forward from
//   //   * @return a sequence of ForecastOut
//   //   */
//   // def forecastData(x0: Vector[State], times: Seq[Time], mod: Model): Seq[ForecastOut] = {
//   //   val samples = x0 map (simDataInit(_, times, mod))

//   //   for {
//   //     sample <- samples.transpose
//   //     t = sample.head.t
//   //     state = sample.map(_.sdeState.get)
//   //     stateIntervals = getAllCredibleIntervals(state, 0.995)
//   //     statemean = meanState(state)
//   //     etas = state map (x => mod.link(mod.f(x, t)))
//   //     meanEta = breeze.stats.mean(etas.map(_.head))
//   //     etaIntervals = getOrderStatistic(etas.map(_.head), 0.995)
//   //     obs = breeze.stats.mean(etas map (mod.observation(_).draw))
//   //     obsIntervals = getOrderStatistic(etas map (mod.observation(_).draw), 0.995)
//   //   } yield ForecastOut(t, obs, obsIntervals, meanEta, etaIntervals, statemean, stateIntervals)
//   // }

//   /**
//     * Performs a one step ahead forecast for any time ahead in the future, given a sample of states, time of the state sample
//     * the time to forecast to and a model
//     * @param x0 a sample of states at the same time point, t0
//     * @param t0 the time the sample of states was made
//     * @param t the time to forcast ahead until
//     * @param mod the model to simulate from
//     * @return a tuple, including a ForecastOut object and the vector of states which has been advanced by dt = t - t0
//     */
//   def oneStepForecast(x0: Seq[State], t0: Time, t: Time, mod: Model): (ForecastOut, Seq[State]) = {
//     val nextStep = x0 map (simStep(_, t0, t - t0, mod))
//     val state = nextStep map (_.sdeState.get)
//     val stateIntervals = getAllCredibleIntervals(state, 0.995)
//     val statemean = meanState(state)
//     val etas = state map (x => mod.link(mod.f(x, t)))
//     val meanEta = breeze.stats.mean(etas.map(_.head))
//     val etaIntervals = getOrderStatistic(etas.map(_.head), 0.995)
//     val obs = breeze.stats.mean(etas map (mod.observation(_).draw))
//     val obsIntervals = getOrderStatistic(etas map (mod.observation(_).draw), 0.995)

//     (ForecastOut(t, obs, obsIntervals, meanEta, etaIntervals, statemean, stateIntervals), state)
//   }

//   /**
//     * Given a stream of times, a model an initial sample of states and the associated time of the state sample
//     * generate a forecast include credible intervals in a stream
//     * @param x0 a vector of state realisations at time t0
//     * @param t0 an initial time which the state was realised at
//     * @param mod the model to simulate from
//     * @return a flow object which can be attached to a Source[Time] and a suitable Sink
//     */
//   def forecastFlow(x0: Seq[State], t0: Time, mod: Model): Flow[Time, ForecastOut, Any] = {
//     Flow[Time].scan(oneStepForecast(x0, t0, t0, mod))((d, t) => oneStepForecast(d._2, d._1.t, t, mod)) map (_._1)
//   }

//   /**
//     * Simulate a single step of an exponential family model
//     * @param x0 the initial state at the time of the last observation
//     * @param t0 the time of the last observation
//     * @param deltat the time increment to the next observation
//     * @param mod a composed model, not the LogGaussiancox model
//     * @return a Data element with the next latent state and observation
//     */
//   def simStep(
//     x0: State,
//     t0: Time,
//     deltat: TimeIncrement,
//     mod: Model): Data = {

//     val x1 = mod.stepFunction(x0, deltat).draw
//     val gamma = mod.f(x1, t0)
//     val eta = mod.link(gamma)
//     val y1 = mod.observation(eta).draw
//     Data(t0, y1, Some(eta), Some(gamma), Some(x1))
//   }

//   /**
//     * Given an initial state, simulate from a model
//     * @param x0 the initial state
//     * @param times a list of times to simulate the data
//     * @param mod an exponential family model
//     * @return a sequence of Data
//     */
//   def simDataInit(x0: State, times: Seq[Time], mod: Model): Seq[Data] = {
//     val d0 = simStep(x0, times.head, 0, mod)

//     val data = times.tail.scanLeft(d0) { (d0, t) =>
//       val deltat = t - d0.t
//       val x0 = d0.sdeState.get
//       simStep(x0, t, deltat, mod)
//     }

//     data.reverse
//   }

//   /**
//     * Simulate data from a list of times, allowing for irregular observations
//     * @param times a list of times to simulate observations at
//     * @param mod an exponential family model
//     * @return a sequence of Data
//     */
//   def simData(times: Seq[Time], mod: Model): Seq[Data] = {

//     val x0 = mod.x0.draw
//     simDataInit(x0, times, mod)
//   }

//   /**
//     * Simulate a step using Rand (a monad representing a distribution which can be sampled from)
//     * @param x0 the initial state at the time of the previous observation
//     * @param t the time of the previous observation
//     * @param deltat the time increment to the time of the next observation
//     * @param mod an exponential family model to simulate from
//     * @return a Rand monad containing data, which can be sampled from to give a realisation from a model
//     */
//   def simStepRand(x0: State, t: Time, deltat: TimeIncrement, mod: Model): Rand[Data] = {
//     for {
//       x1 <- mod.stepFunction(x0, deltat)
//       gamma = mod.f(x1, t)
//       eta = mod.link(gamma)
//       y1 <- mod.observation(eta)
//     } yield Data(t, y1, Some(eta), Some(gamma), Some(x1))
//   }

//   /**
//     * Simulate a vector of Data using Rand (a monad representing a distribution which can be sampled from)
//     * @param times a list of times to observe the process at
//     * @param mod an exponential family model
//     * @return a Rand monad containing a vector of data
//     */
//   def simDataRand(times: Seq[Time], mod: Model): Rand[Seq[Data]] = {
//     val x0 = mod.x0.draw
//     val init = simStepRand(x0, times.head, 0, mod)

//     val data = times.tail.scanLeft(init) { (d, t) => 
//       for {
//         d0 <- d
//         deltat = t - d0.t
//         x0 = d0.sdeState.get
//         d1 <- simStepRand(x0, t, deltat, mod)
//       } yield d1
//     }

//     sequence(data.reverse)
//   }

//   /**
//     * Simulate data as an Akka Stream, with regular time intervals
//     * @param mod The model to simulate from, can be composed or single
//     * @param precision Used to determine the step length, dt = 10^-precision
//     * @param t0 the starting time of the process
//     * @return a source of an Akka Stream
//     */
//   def simStream(mod: Model, precision: Int, t0: Time): Source[Data, Any] = {
//     val dt = math.pow(10, -precision)

//     val x0 = mod.x0.draw
//     val initialObservation = simStep(x0, t0, 0, mod)

//     Source.unfold(initialObservation){d => Some((simStep(d.sdeState.get, d.t + dt, dt, mod), d)) }
//   }
// }
