package model

import breeze.numerics.{exp, log}
import breeze.stats.distributions.{Gaussian, Uniform, Exponential, Rand, ContinuousDistr, Gamma, Multinomial, MultivariateGaussian}
import breeze.linalg.{linspace, DenseVector, DenseMatrix, diag}
import breeze.stats.{mean, variance}
import scala.collection.parallel.immutable.ParVector

import model.POMP._
import model.Utilities._
import model.DataTypes._
import model.State._
import model.SimData._

import akka.stream.scaladsl.Source
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.File
import akka.stream.scaladsl._
import akka.util.ByteString

object Filtering {

  /**
    * Return a vector of lag 1 time differences
    * @param x a list of times
    * @return a list of differenced times
    */
  def diff(x: Iterable[Time]): Iterable[TimeIncrement] = {
    (x.tail zip x) map { a => a._1 - a._2 }
  }

  /**
    * This samples integers from 1 to n with replacement according to their associated probabilities
    * @param n a number matching the number of probabilities
    * @param prob a vector of probabilities corresponding to the probability of sampling that integer
    * @return a vector containing the samples
    */
  def sample(
    n: Int,
    prob: DenseVector[Double]): Vector[Int] = {
    Multinomial(prob).sample(n).toVector
  }

  /**
    * Returns a function closure from parameters to an estimate of the marginal log likelihood of a path
    * @param n the number of particles
    * @param data a vector of data
    * @param mod a POMP model
    * Can we test the speed difference between recursive and fold version
    */
  def pfMllFold(
    data: Vector[Data],
    unparamMod: Parameters => Model)(
    n: Int): Parameters => LogLikelihood = { p =>

    val mod = unparamMod(p)
    val times = data.map(_.t)
    val deltas = diff(times.head +: times).iterator
    val x0 = Vector.fill(n)(mod.x0.draw)

    data.foldLeft((x0, 0.0))((a, y) => {
      val (state, ll) = a

      // take the next time difference
      val dt = deltas.next

      // advance state particles
      val x1 = state map(x => mod.stepFunction(x, dt).draw)

      // Calculate the ll of the propagation of particles given the observation
      val w1: Vector[LogLikelihood] = x1 map (a => mod.dataLikelihood(mod.link(mod.f(a, y.t)), y.observation))

      val max = w1.max // log-sum-exp trick
      val w = w1 map { a => exp(a - max) }

      val xInd = sample(n, DenseVector(w.toArray)) // Multinomial resampling
      val x2 = xInd map ( x1(_) )

      (x2, ll + max + log(breeze.stats.mean(w)))
    })._2
  }

  def pfMll(
    data: Vector[Data],
    unparamMod: Parameters => Model)(
    n: Int): Parameters => LogLikelihood = { p => 

    val mod = unparamMod(p)
    val times = data.map(_.t)
    val dt = diff(times.head +: times)
    val initState = Vector.fill(n)(mod.x0.draw)

    def go(
      observations: Vector[Data],
      x0: Vector[State],
      ll: LogLikelihood,
      deltas: Iterable[Time]): LogLikelihood = observations match {
      case IndexedSeq() => ll
      case y +: ys => 
        val x1 = x0 map (x => mod.stepFunction(x, deltas.head).draw)

        val likelihoodState = x1 map (x => mod.link(mod.f(x, y.t)))

        val w1 = likelihoodState map (l => mod.dataLikelihood(l, y.observation))

        val max = w1.max
        val w = w1 map { a => exp(a - max) }

        val xInd = sample(n, DenseVector(w.toArray))
        val x2 = xInd map ( x1(_) )

        go(ys, x2, ll + max + log(breeze.stats.mean(w)), deltas.tail)
    }

    go(data, initState, 0.0, dt)
  }

  def mean(it: ParVector[Double]): Double = {
    it.sum / it.size
  }

  def pfMllPar(
    data: Vector[Data],
    unparamMod: Parameters => Model)(
    n: Int): Parameters => LogLikelihood = { p =>

    val mod = unparamMod(p)
    val times = data.map(_.t)
    val deltas = diff(times.head +: times).iterator
    val x0: ParVector[State] = Vector.fill(n)(mod.x0.draw).par

    data.foldLeft((x0, 0.0))((a, y) => {
      val (state, ll) = a

      // Choose next time
      val dt = deltas.next

      // advance state particles
      val x1 = state map(x => mod.stepFunction(x, dt).draw)

      // Calculate the ll of the propagation of particles given the observation
      val w1 = x1 map (a => mod.dataLikelihood(mod.link(mod.f(a, y.t)), y.observation))

      val max = w1.max // log-sum-exp trick
      val w = w1 map { a => exp(a - max) }

      val xInd = sample(n, DenseVector(w.toArray)).par // Multinomial resampling
      val x2 = xInd map ( x1(_) )

      (x2, ll + max + log(mean(w)))
    })._2
  }

  /**
    * Calculate the importance weights for the Log-Gaussian Cox-Process Bootstrap Filter
    * @param t is the time of the event
    * @param t0 is the time of the last event
    * @param x0 is the state at time t0
    * @param precision determines the step size of the SDE
    * @param mod the model
    * @p the parameters of the model
    * @return the log likelihood of an event occurring at time t0 given the parameters, the 
    * state is 
    */
  def importanceResampleWeights(
    t0: Time,
    t: Time,
    x0: State,
    precision: Int,
    y: Observation,
    mod: Model): (LogLikelihood, Stream[State]) = {

    // Step length
    val dt = Math.pow(10, -precision)

    // SDE Stream
    val x = simSdeStream(x0, t0, t - t0, precision, mod.stepFunction).
      takeWhile(a => a.time <= t)

    // add deterministic transformation
    val transformedState = x map (a => mod.f(a.state, a.time))

    // sum the transformed state to calculate the cumulative hazard and return the value of the state at time t
    val likelihoodState = Vector(transformedState.last, transformedState.map(x => exp(x) * dt).sum)

    val likelihood = mod.dataLikelihood(likelihoodState, y)

    // we return the likelihood of the observation and the entire state sampled between t0 and t
    (likelihood, x.map(_.state))
  }

  def pfLGCPmll(
    data: Vector[Data],
    unparamMod: Parameters => Model,
    precision: Int)(n: Int): Parameters => LogLikelihood = p => {

    // parameterise the model
    val mod = unparamMod(p)
    val particles = Vector.fill(n)(mod.x0.draw)

    def loop(
      x0: Vector[State],
      t0: Time,
      yy: Vector[Data],
      ll: LogLikelihood): LogLikelihood = yy match {
      case IndexedSeq() => ll
      case y +: ys =>

        val weightsState = x0 map (x =>
          importanceResampleWeights(t0, y.t, x, precision, y.observation, mod))

        val x1 = weightsState map (_._2)
        val w1 = weightsState map (_._1)

       // all intermediate states in a vector of streams
        // we could return these if we wanted to interpolate the state prediction
    
        // log-sum-exp trick
        val max = w1.max 
        val w = w1 map { a => exp(a - max) }

        // Resample the hazard function according to how likely
        // it is to have produced an observation at time "t"
        val xInd = sample(n, DenseVector(w.toArray))
        val x2: Vector[Stream[State]] = xInd map ( x1(_) )

        loop(x2.map(_.last), y.t, ys, ll + max + log(breeze.stats.mean(w)))

    }
    // initialise with a draw from the model x0 and time 0
    loop(particles, 0, data, 0)
  }

  /** An lgcp particle filter, which utilises streaming in between observations to calculate the cumulative hazard
    * This could work generically, as a particle filter on a grid (maybe)
    * This does work generically
    */
  def pfLGCP(
    n: Int,
    data: Vector[Data],
    unparamMod: Parameters => Model,
    precision: Int): Parameters => Vector[PfOut] = p => {

    val mod = unparamMod(p)
    val particles = Vector.fill(n)(mod.x0.draw)

    def loop(
      x0: Vector[State],
      t0: Time,
      yy: Vector[Data],
      acc: Vector[PfOut]): Vector[PfOut] = yy match {
      case IndexedSeq() => acc.reverse
      case y +: ys =>

        // calculate the importance weights and state from time t0 to y.t stepped by precision
        val weightsState = x0 map (x =>
          importanceResampleWeights(t0, y.t, x, precision, y.observation, mod))

        val x1 = weightsState map (_._2)
        val w1 = weightsState map (_._1)

        // log-sum-exp trick
        val max = w1.max 
        val w = w1 map { a => exp(a - max) }

        // Resample the hazard function according to how likely
        // it is to have produced an observation at time "t"
        val xInd = sample(n, DenseVector(w.toArray))
        val x2: Vector[Stream[State]] = xInd map ( x1(_) )

        loop(x2.map(_.last), y.t, ys,
          PfOut(y.t, Some(1.0), weightedMean(x2.map(_.last), w), getAllCredibleIntervals(x2.map(_.last), 0.99)) +: acc)

    }
    // initialise with a draw from the model x0 and time 0
    loop(particles, 0, data, Vector())
  }

  /**
    * The Bootstrap particle filter with credible intervals for each state
    * @param n number of particles
    * @param data observation data, with time
    * @param mod a suitable model for the observed data
    * @return the estimated state and credible intervals
    */
  def bootstrapPf(
    n: Int,
    data: Vector[Data],
    unparamMod: Parameters => Model): Parameters => Vector[PfOut] = p => {

    // parameterise the model, select times and initialise
    val mod = unparamMod(p)
    val times = data map (_.t)
    val particles = Vector.fill(n)(mod.x0.draw)
    val dts = diff(times.head +: times)

    def loop(
      x0: Vector[State],
      yy: Vector[Data],
      states: Vector[State],
      acc: Vector[PfOut], deltas: Iterable[TimeIncrement]): Vector[PfOut] = yy match {

      case IndexedSeq() => acc.reverse
      case y +: ys =>
        val x1 = x0 map(x => mod.stepFunction(x, deltas.head).draw)

        // Calculate the transformed state: seasonality etc
        val transformedState = x1 map (a => mod.link(mod.f(a, y.t)))

        // Calculate the ll of the propagation of particles given the observation
        val w1: Vector[LogLikelihood] = transformedState map (a => mod.dataLikelihood(a, y.observation))

        val max = w1.max // log sum exp
        val w = w1 map { a => exp(a - max) }

        val xInd = sample(n, DenseVector(w.toArray)) // Multinomial resampling
        val x2 = xInd map ( x1(_) )
        
        loop(x2, ys,
          states :+ weightedMean(x2, w),
          PfOut(y.t,
            Some(y.observation),
            weightedMean(x2, w),
            getAllCredibleIntervals(x2, 0.99)) +: acc,
          deltas.tail)
    }

    loop(particles, data, Vector(), Vector(), dts)
  }

  /**
    * A class to monitor the state of ann MCMC chain
    * @param i the number of iterations computed
    * @param v the variance of the estimate of the marginal log-likelihood estimate
    * @param a the proportion of accepted moves
    */
  case class MonitorState(i: Int, v: Double, a: Double)

  /**
    * A helper function to monitor the stream every 'every' iterations with a print statement
    * @param every number of iterations to wait until a print statement informs of the iteration, mll variance and acceptance ratio
    * @param chain, the chain number we are monitoring
    */
  def monitorStream(every: Int, chain: Int) = {
    Flow[MetropState].
          zip(Source(Stream.from(1))).
          grouped(1000).
          map( x => {
            val iter = x map (_._2)
            val ll = x map (_._1.ll)
            MonitorState(
              iter.last,
              variance(ll),
              (x.map(_._1.accepted.toDouble).last)/iter.last)}
          ).
          map(m => println(s"""chain: $chain, iteration: ${m.i}, mll Variance: ${m.v}, acceptance ratio: ${m.a}"""))
  }

  /**
    * Run the PMMH algorithm, with multiple chains
    */
  def runPmmhToFile(
    fileOut: String, chains: Int,
    initParams: Parameters, mll: Int => Parameters => LogLikelihood,
    perturb: Parameters => Rand[Parameters], particles: Int, iterations: Int): Unit = {

    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val system = ActorSystem("StreamingPMMH")
    implicit val materializer = ActorMaterializer()

    Source(1 to chains).
      mapAsync(parallelism = 4){ chain =>
        val iters = ParticleMetropolis(mll(particles), perturb).iters(initParams)

        println(s"""Running chain $chain, with $particles particles, $iterations iterations""")

        iters.
          zip(Source(Stream.from(1))).
          map{ case (x, i) => (i, x.params) }.
          take(iterations).
          map{ case (i, p) => ByteString(s"$i, $p\n") }.
          runWith(FileIO.toFile(new File(s"$fileOut-$iterations-$particles-$chain.csv")))
  
        iters.
          via(monitorStream(1000, chain)).
          runWith(Sink.ignore)
      }.
      runWith(Sink.onComplete { _ =>
        system.shutdown()
      })
  }

  /**
    * Perturbs the parameters of a Stochastic Differential Equation
    */
  def sdePerturb(delta: Double, logdelta: Double): SdeParameter => Rand[SdeParameter] = p => p match {
    case BrownianParameter(m, s) => new Rand[SdeParameter] {
      def draw = {
        // s is non-negative, propose on log scale
        BrownianParameter(
          m map(_ + Gaussian(0, delta).draw),
          s map (x => x * exp(Gaussian(0, logdelta).draw)))
      }
    }
    case OrnsteinParameter(theta, alpha, sigma) => new Rand[SdeParameter] {
      def draw = {
        // alpha and sigme are non-negative, propose on log-scale
        OrnsteinParameter(
          theta map (Gaussian(_, delta).draw),
          alpha map (x => x * exp(Gaussian(0, logdelta).draw)),
          sigma map (x => x * exp(Gaussian(0, logdelta).draw)))
      }
    }
    case StepConstantParameter(a) =>
      new Rand[SdeParameter] {
        def draw = StepConstantParameter(
          a map (Gaussian(_, delta).draw))
      }
  }

  /**
    * Perturbs the initial parameters of the state space
    */
  def initParamPerturb(delta: Double, logDelta: Double): StateParameter => Rand[StateParameter] = p => p match {
    case GaussianParameter(m, s) => new Rand[StateParameter] {
      def draw = {
        GaussianParameter(
          m map (Gaussian(_, delta).draw),
          diag(diag(s) map (x => x * exp(Gaussian(0, logDelta).draw))))
      }
    }
  }

  /**
    * Takes a tree of parameters, changes the value at the leaves of all of them a small amount,
    * then returns a parameter tree with the same structure
    * @return a parameter tree with the same structure, but different values
    */
  def gaussianPerturb(delta: Double, logdelta: Double): (Parameters) => Rand[Parameters] = p => p match {
    case LeafParameter(initParams, v, sde) => 
      for {
        init <- initParamPerturb(delta, logdelta)(initParams)
        sde <- sdePerturb(delta, logdelta)(sde)
      } yield LeafParameter(init, v map (x => x * exp(Gaussian(0, logdelta).draw)), sde)
      
    case BranchParameter(lp, rp) =>
      for {
        l <- gaussianPerturb(delta, logdelta)(lp)
        r <- gaussianPerturb(delta, logdelta)(rp)
      } yield BranchParameter(l, r)
  }

  /**
    * A random walk draw from a multivariate gaussian distribution
    */
  def mvnPropose(covariance: DenseMatrix[Double]): Parameters => Rand[Parameters] = ???

  //   val noise = MultivariateGaussian(DenseVector.zeros[Double](covariance.rows), covariance).draw
  // }

  /**
    * A diagnostic function for the PMMH algorithm
    */
  def noPerturb: Parameters => Rand[Parameters] = p => new Rand[Parameters] { def draw = p }

  /**
    * Get the credible intervals of the nth state vector
    * @param s a State
    * @param n a reference to a node of state tree, counting from 0 on the left
    * @param interval the probability interval size
    * @return a tuple of doubles, (lower, upper)

    */
  def credibleIntervals(s: Vector[State], n: Int, interval: Double): IndexedSeq[CredibleInterval] = {
    val state: Vector[LeafState] = s map (State.getState(_, n)) // Gets the nth state vector
    val stateVec = state.head.x.indices map (i => state.map(a => a.x(i)))
    stateVec map (a => {
      val index = Math.floor(interval * a.length).toInt
      val stateSorted = a.sorted
      CredibleInterval(stateSorted(a.length - index - 1), stateSorted(index - 1))
    })
  }

  /**
    * Use credible intervals to get all credible intervals of a state
    * @param s a vector of states
    * @param interval the interval for the probability interval between [0,1]
    * @return a sequence of tuples, (lower, upper) corresponding to each state reading
    */
  def getAllCredibleIntervals(s: Vector[State], interval: Double): IndexedSeq[CredibleInterval] = {
    State.toList(s.head).indices.flatMap(i => credibleIntervals(s, i, interval))
  }
}
