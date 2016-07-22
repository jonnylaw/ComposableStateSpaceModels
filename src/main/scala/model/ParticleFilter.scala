package model

import model.POMP._
import model.Utilities._
import model.DataTypes._
import model.State._
import model.SimData._
import scala.language.higherKinds._

import breeze.stats.distributions.{Rand, Uniform, Multinomial}
import breeze.stats.distributions.Rand._
import breeze.numerics.exp
import breeze.linalg.DenseVector
import ParticleFilter._
import Filtering._

import akka.stream.scaladsl.Source
import akka.stream.scaladsl._

// TODO:
// * Implement a parallel particle filter (just use a par vec)

/**
  * Representation of the state of the particle filter, at each step the previous observation time, t0, and 
  * particle cloud, particles, is required to compute forward.
  * The meanState and intervals are recorded in each step, so they can be outputted immediately without having
  * to calculate these from the particle cloud after
  */
case class PfState(
  t: Time,
  observation: Option[Observation],
  particles: Vector[State],
  weights: Vector[LogLikelihood],
  ll: LogLikelihood) {

   override def toString = observation match {
      case Some(y) => s"$t, $y, ${weightedMean(particles, weights).flatten.mkString(", ")}"
      case None => s"$t, ${weightedMean(particles, weights).flatten.mkString(", ")}"
    }

}

trait ParticleFilter {

  val unparamMod: Parameters => Model
  val t0: Time

  def advanceState(x: Vector[State], dt: TimeIncrement, t: Time)(p: Parameters): Rand[Vector[(State, Eta)]]
  def calculateWeights(x: Eta, y: Observation)(p: Parameters): LogLikelihood
  def resample: Resample[State]

  /**
    * Perform one step of a particle filter
    * @param y a single timestamped observation
    * @param s the state of the particle filter at the time of the last observation
    * @return the state of the particle filter after observation y
    */
  def stepFilter(y: Data, s: PfState)(p: Parameters): Rand[PfState] = {
    val dt = y.t - s.t // calculate time between observations

    for {
      x <- advanceState(s.particles, dt, y.t)(p)
      (x1, eta) = x.unzip
      w = eta map (a => calculateWeights(a, y.observation)(p))
      max = w.max
      w1 = w map { a => exp(a - max) }
      x2 = resample(x1, w1)
      ll = s.ll + max + math.log(breeze.stats.mean(w1))
    } yield PfState(y.t, Some(y.observation), x2, w1, ll)
  }

  def llFilter(data: Vector[Data])(particles: Int)(p: Parameters): Rand[LogLikelihood] = {
    val mod = unparamMod(p)
    val initState: PfState = PfState(t0, None, Vector.fill(particles)(mod.x0.draw), Vector.fill(particles)(1.0), 0.0)

    data.foldLeft(always(initState))((s, y) => s flatMap (x => stepFilter(y, x)(p))) map (_.ll)
  }

  /**
    * Run a filter over a vector of data and return a vector of PfState
    */
  def accFilter(data: Vector[Data])(particles: Int)(p: Parameters): Rand[Vector[PfState]] = {
    val mod = unparamMod(p)
    val initState: PfState = PfState(t0, None, Vector.fill(particles)(mod.x0.draw), Vector.fill(particles)(1.0), 0.0)

    val x = data.
      foldLeft(Vector(always(initState)))(
        (acc, y) => (acc.head flatMap (x => stepFilter(y, x)(p))) +: acc)

    sequence(x.reverse.tail)
  }

  /**
    * Run a filter over a stream of data
    */
  def filter(data: Source[Data, Any])(particles: Int)(p: Parameters): Source[Rand[PfState], Any] = {
    val mod = unparamMod(p)
    val initState: PfState = PfState(t0, None, Vector.fill(particles)(mod.x0.draw), Vector.fill(particles)(1.0), 0.0)

    data.scan(always(initState))((s, y) => s flatMap (x => stepFilter(y, x)(p)))
  }
}

object ParticleFilter {
  type Resample[A] = (Vector[A], Vector[LogLikelihood]) => Vector[A]

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
  def sample(n: Int, prob: DenseVector[Double]): Vector[Int] = {
    Multinomial(prob).sample(n).toVector
  }

  /**
    * Given a vector of doubles, returns a normalised vector with probabilities summing to one
    * @param prob a vector of unnormalised probabilities
    * @return a vector of normalised probabilities
    */
  def normalise(prob: Vector[Double]): Vector[Double] = {
    prob map (_/prob.sum)
  }

  def cumsum(x: Vector[Double]): Vector[Double] = {
    val sums = x.foldLeft(Vector(0.0))((acc: Vector[Double], num: Double) => (acc.head + num) +: acc)
    sums.reverse.tail
  }

  /**
    * Multinomial Resampling, sample from a categorical distribution with probabilities
    * equal to the particle weights 
    */
  def multinomialResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]): Vector[A] = {
    val indices = sample(particles.size, DenseVector(weights.toArray))
    indices map { particles(_) }
  }

  /**
    * Produces a histogram output of a vector of Data
    */
  def hist(x: Vector[Int]): Unit = {
    val h = x.
      groupBy(identity).
      toVector.map{ case (n, l) => (n, l.length) }.
      sortBy(_._1)

    h foreach { case (n, count) => println(s"$n: ${Vector.fill(count)("#").mkString("")}") }
  }

  /**
    * Return the value x such that, F(p) = x, where F is the empirical cumulative distribution function over
    * the particles
    */
  def invecdf[A](ecdf: Vector[(A, LogLikelihood)], p: Double): A = {
    ecdf.
      filter{ case (_, w) => w > p }.
      map{ case (x, _) => x }.head
  }

  /**
    * Stratified resampling
    * Sample n ORDERED uniform random numbers (one for each particle) using a linear transformation of a U(0,1) RV
    */
  def stratifiedResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]): Vector[A] = {
    // generate n uniform random numbers
    val n = weights.length
    val u = (1 to n).map(k => (k - 1 + Uniform(0,1).draw) / n).toVector
    val ecdf = particles.zip(cumsum(normalise(weights)))

    u map (invecdf(ecdf, _))
  }

  /**
    * Systematic Resampling
    * Sample n ORDERED numbers (one for each particle), reusing the same U(0,1) variable
    */
  def systematicResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]): Vector[A] = {
    val n = weights.length
    val u = Uniform(0,1).draw
    val k = (1 to n).map(a => (a - 1 + u) / n).toVector
    val ecdf = particles.zip(cumsum(normalise(weights)))

    k map (invecdf(ecdf, _))
  }

  /**
    * Residual Resampling
    * Select particles in proportion to their weights, ie particle xi appears ki = n * wi times
    * Resample m (= n - total allocated particles) particles according to w = n * wi - ki using other resampling technique
    */
  def residualResampling[A](particles: Vector[A], weights: Vector[LogLikelihood]): Vector[A] = {
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

    val i = sample(m, DenseVector(residualWeights.toArray))
    x ++ (i map { particles(_) })
  }

  /**
    * map2 implementation for Rand
    */
  def map2[A,B,C](ma: Rand[A], mb: Rand[B])(f: (A, B) => C): Rand[C] = {
    for {
      a <- ma
      b <- mb
    } yield f(a, b)
  }

  /**
    * Traverse method for Rand and Vector
    */
  def traverse[A,B](l: Vector[A])(f: A => Rand[B]): Rand[Vector[B]] = {
    l.foldRight(always(Vector[B]()))((a, mlb) => map2(f(a), mlb)(_ +: _))
  }

  /**
    * Sequence, Traverse with the identity
    */
  def sequence[A](l: Vector[Rand[A]]): Rand[Vector[A]] = {
    traverse(l)(a => a)
  }
}

case class Filter(model: Parameters => Model, resamplingScheme: Resample[State], t0: Time) extends ParticleFilter {
  
  val unparamMod = model

  def advanceStateHelp(x: State, dt: TimeIncrement, t: Time)(p: Parameters): Rand[(State, Eta)] = {
    val mod = unparamMod(p)
    for {
      x1 <- mod.stepFunction(x, dt)
      eta = mod.link(mod.f(x1, t))
    } yield (x1, eta)
  }

  def advanceState(x: Vector[State], dt: TimeIncrement, t: Time)(p: Parameters): Rand[Vector[(State, Eta)]] = {
    val mod = unparamMod(p)

    traverse(x)(advanceStateHelp(_, dt, t)(p))
  }

  def calculateWeights(x: Eta, y: Observation)(p: Parameters): LogLikelihood = {
    val mod = unparamMod(p)
    mod.observation(x).logApply(y)
  }

  def resample: Resample[State] = resamplingScheme
}

/**
  * In order to calculate Eta in the LGCP model, we need to merge the advance state and transform state functions
  */
case class FilterLgcp(model: Parameters => Model, resamplingScheme: Resample[State], precision: Int, t0: Time) extends ParticleFilter {

  val unparamMod = model

  def calcWeight(x: State, dt: TimeIncrement, t: Time)(p: Parameters): Rand[(State, Eta)] = new Rand[(State, Eta)] {
    def draw = {
      val mod = unparamMod(p)
      val x1 = simSdeStream(x, t - dt, dt, precision, mod.stepFunction)
      val transformedState = x1 map (a => mod.f(a.state, a.time))

      (x1.last.state, Vector(transformedState.last, transformedState.map(x => exp(x) * dt).sum))
    }
  }

  def advanceState(x: Vector[State], dt: TimeIncrement, t: Time)(p: Parameters): Rand[Vector[(State, Eta)]] = {
    traverse(x)(calcWeight(_, dt, t)(p))
  }

  def calculateWeights(x: Eta, y: Observation)(p: Parameters): LogLikelihood = {
    val mod = unparamMod(p)
    mod.observation(x).logApply(y)
  }

  def resample: Resample[State] = resamplingScheme
}
