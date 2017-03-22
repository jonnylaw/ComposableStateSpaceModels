package com.github.jonnylaw.model

import breeze.stats.distributions._
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import breeze.numerics.{sqrt, exp}
import cats.{Semigroup, Applicative}
import cats.data.Reader
import cats.implicits._
import akka.stream._
import akka.stream.scaladsl._

trait Sde { self =>
  def initialState: Rand[State]
  def drift(state: State): Tree[DenseVector[Double]]
  def diffusion(state: State): Tree[DenseMatrix[Double]]
  def dimension: Int

  /**
    * Exact transition density of an SDE, if this exists, otherwise the default implementation
    * is the Euler Maruyama method
    */
  def stepFunction(dt: TimeIncrement)(s: State)(implicit rand: RandBasis = Rand): Rand[State] = {
    stepEulerMaruyama(dt)(s)
  }

  /**
    * A Wiener Process, given a time increment it generates a vector of Gaussian(0, dt) 
    */
  def dW(dt: TimeIncrement)(implicit rand: RandBasis = Rand): Rand[State] = {
    val sample = DenseMatrix.eye[Double](dimension) * sqrt(dt) * DenseVector.rand(dimension, rand.gaussian(0, 1))
    Rand.always(Tree.leaf(sample))
  }

  // Approximate solution
  def stepEulerMaruyama(dt: TimeIncrement)(s: State): Rand[State] = {
    for {
      wiener <- dW(dt)
      a = drift(s).map(_ * dt)
      b = diffusion(s).zipWith(wiener)(_ * _)
      newState = s.zipWith(a)(_+_).zipWith(b)(_ + _)
    } yield newState
  }

  def simEuler(t0: Time, dt: TimeIncrement) = {
    MarkovChain(initialState.draw)(stepEulerMaruyama(dt))
  }

  def simProcess(t0: Time, dt: TimeIncrement) = {
    MarkovChain(initialState.draw)(stepFunction(dt))
  }

  def simStream(t0: Time, dt: TimeIncrement) = {
    Source.fromIterator(() => simProcess(t0, dt).steps)
  }

  def simInit(t0: Time, initialState: State, dt: TimeIncrement) = {
    MarkovChain(StateSpace(t0, initialState))(s => for {
      x <- stepFunction(dt)(s.state)
      t = s.time + dt
    } yield StateSpace(t, x))
  }

  def simInitStream(t0: Time, initialState: State, dt: TimeIncrement): Stream[StateSpace] = {
    simInit(t0, initialState, dt).steps.toStream
  }
}

private final case class BrownianMotion(p: BrownianParameter, dimension: Int) extends Sde {
  val params = BrownianParameter(p.m0, exp(p.c0), p.mu, exp(p.sigma))

  def initialState: Rand[State] = {
    val res: Rand[List[Double]] = Applicative[Rand].replicateA(dimension, Gaussian(params.m0, params.c0))
    res.map(x => Tree.leaf(DenseVector(x.toArray)))
  }

  def drift(state: State) = Tree.leaf(DenseVector.fill(dimension)(params.mu))

  def diffusion(state: State) = Tree.leaf(DenseMatrix.eye[Double](1) * params.sigma)

  override def stepFunction(dt: TimeIncrement)(s: State)(implicit rand: RandBasis = Rand) = {
    val res = s map { x => 
      val mean = x + params.mu * dt
      val varianceMatrix = DenseMatrix.eye[Double](dimension) * sqrt(params.sigma * dt)

      varianceMatrix * DenseVector.rand(dimension, rand.gaussian(0, 1)) + mean
    }

    Rand.always(res)
  }
}

/**
  * The Ornstein-Uhlenbeck process, with specified dimension, with alpha, theta and sigma the same in each dimension
  */
private final case class OuProcess(p: OuParameter, dimension: Int) extends Sde {
  val params = OuParameter(p.m0, exp(p.c0), p.theta, exp(p.alpha), exp(p.sigma))

  override def stepFunction(dt: TimeIncrement)(s: State)(implicit rand: RandBasis = Rand) = { 
    val mean: State = s map (x =>  (x :- params.theta) * exp(- params.alpha * dt) :+ params.theta)
    val variance = (params.sigma * params.sigma/ 2 * params.alpha ) * (1 - exp(-2 * params.alpha * dt))

    // the cholesky decomposition of a diagonal matrix is just the square root of the diagonal
    val varianceMatrix = DenseMatrix.eye[Double](dimension) * sqrt(variance)

    val res = mean map (m => m + varianceMatrix * DenseVector.rand(dimension, rand.gaussian(0, 1)))
    Rand.always(res)
  }

  def initialState: Rand[State] = {
    val res: Rand[List[Double]] = Applicative[Rand].replicateA(dimension, Gaussian(params.m0, params.c0))
    res.map(x => Tree.leaf(DenseVector(x.toArray)))
  }

  def drift(state: State) = {
    state map (x => params.alpha * (params.theta - x))
  }

  def diffusion(state: State) = Tree.leaf(DenseMatrix.eye[Double](dimension) * params.sigma)
}

/**
  * Representing a realisation from a stochastic differential equation
  * @param time
  * @param state
  */
case class StateSpace(time: Time, state: State) {
  override def toString = time + "," + state.toString
}

object Sde {
  def brownianMotion(dimension: Int): SdeParameter => Sde = p => p match {
    case param: BrownianParameter =>
      BrownianMotion(param, dimension)
    case _ => throw new Exception(s"Incorrect parameters supplied to Brownianmotion, expected BrownianParameter, received $p")
  }

  /**
    * The Ornstein Uhlenbeck Process with mean theta, mean reverting parameter alpha > 0 and diffusion sigma > 0
    * The dimension is controlled by an explicit dimension parameter, the parameters, theta, alpha and sigma are recycled
    * for each dimension required
    * The parameters should be specified on a log scale.
    * @param dimension the dimension of the diffusion process
    * @return a function from SdeParameter => Sde 
    */
  def ouProcess(dimension: Int): SdeParameter => Sde = p => p match {
    case param: OuParameter =>
      OuProcess(param, dimension)
    case _ => throw new Exception(s"Incorrect parameters supplied to OuProcess, expected OuParameter, received $p")
  }

  implicit def sdeSemigroup = new Semigroup[Sde] {
    def combine(sde1: Sde, sde2: Sde): Sde = new Sde {
      def initialState: Rand[State] = for {
        l <-sde1.initialState
        r <- sde2.initialState
      } yield Tree.branch(l, r)

      def drift(state: State): Tree[DenseVector[Double]] = state match {
        case Branch(l, r) => Tree.branch(sde1.drift(l), sde2.drift(r))
        case state: Leaf[DenseVector[Double]] => throw new Exception
      }

      def diffusion(state: State): Tree[DenseMatrix[Double]] = state match {
        case Branch(l, r) => Tree.branch(sde1.diffusion(l), sde2.diffusion(r))
        case state: Leaf[DenseVector[Double]] => throw new Exception
      }

      override def stepFunction(dt: TimeIncrement)(s: State)(implicit rand: RandBasis = Rand) = s match {
        case Branch(l, r) => for {
          ls <- sde1.stepFunction(dt)(l)
          rs <- sde2.stepFunction(dt)(r)
        } yield Tree.branch(ls, rs)
        case _ => throw new Exception
      }

      def dimension: Int = sde1.dimension + sde2.dimension

      override def dW(dt: TimeIncrement)(implicit rand: RandBasis = Rand): Rand[Tree[DenseVector[Double]]] = {
        for {
          l <- sde1.dW(dt)
          r <- Applicative[Rand].replicateA(sde2.dimension, Gaussian(0.0, sqrt(dt))).map(x => (DenseVector(x.toArray)))
        } yield l |+ Tree.leaf(r)
      }
    }
  }
}
