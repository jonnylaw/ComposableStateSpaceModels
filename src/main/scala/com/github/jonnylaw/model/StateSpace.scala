package com.github.jonnylaw.model

import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian, MarkovChain, Process}
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import breeze.numerics.{sqrt, exp}
import cats.{Semigroup, Applicative}
import cats.data.Reader
import cats.implicits._
import fs2._

trait Sde { self =>
  def initialState: Rand[State]
  def drift(state: State): Tree[DenseVector[Double]]
  def diffusion(state: State): Tree[DenseMatrix[Double]]
  def dimension: Int

  /**
    * Exact transition density of an SDE, if this exists, otherwise the default implementation
    * is the Euler Maruyama method
    */
  def stepFunction(dt: TimeIncrement)(s: State): Rand[State] = {
    stepEulerMaruyama(dt)(s)
  }

  /**
    * A Wiener Process, given a time increment it generates a vector of Gaussian(0, dt) 
    */
  def dW(dt: TimeIncrement): Rand[Tree[DenseVector[Double]]] = {
    Applicative[Rand].replicateA(dimension, Gaussian(0.0, sqrt(dt))).
      map(x => Tree.leaf(DenseVector(x.toArray)))
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

  def simEuler(t0: Time, dt: TimeIncrement): Process[State] = {
    MarkovChain(initialState.draw)(stepEulerMaruyama(dt))
  }

  def simProcess(t0: Time, dt: TimeIncrement): Process[State] = {
    MarkovChain(initialState.draw)(stepFunction(dt))
  }

  def simStream[F[_]](t0: Time, dt: TimeIncrement): Stream[F, State] = {
    fromProcess[F, State](simProcess(t0, dt))
  }
}

private final case class BrownianMotion(p: BrownianParameter) extends Sde {

  def dimension: Int = p.mu.size
  def initialState: Rand[State] = MultivariateGaussian(p.m0, p.c0) map Tree.leaf
  def drift(state: State) = Tree.leaf(p.mu)
  def diffusion(state: State) = Tree.leaf(p.sigma)

  override def stepFunction(dt: TimeIncrement)(s: State) = {
    val res = s map (x => DenseVector((x.data, p.mu.data, diag(p.sigma).toArray).zipped.
      map { case (a: Double, m: Double, sd: Double) =>
        Gaussian(a + m * dt, Math.sqrt(sd * sd * dt)).draw
      }))
    Rand.always(res)
  }
}

private final case class OrnsteinUhlenbeck(p: OrnsteinParameter) extends Sde {
  override def stepFunction(dt: TimeIncrement)(s: State) = {
    val res: State = s map { x => 
      val mean = (x.data, p.alpha.data, p.theta.data).zipped map { case (state, a, t) => t + (state - t) * exp(- a * dt) }
      val variance = (p.sigma.data, p.alpha.data).zipped map { case (s, a) => (s*s/2*a)*(1-exp(-2*a*dt)) }
      DenseVector(mean.zip(variance) map { case (a, v) => Gaussian(a, sqrt(v)).draw() })
    }
    Rand.always(res)
  }

  def dimension: Int = p.theta.size
  def initialState: Rand[State] = MultivariateGaussian(p.m0, p.c0) map Tree.leaf
  def drift(state: State) = {
    val c = state map (x => p.theta - x)
    c map ((x: DenseVector[Double]) => diag(p.alpha) * x)
  }
  def diffusion(state: State) = Tree.leaf(diag(p.sigma))
}

/**
  * Representing a realisation from a stochastic differential equation
  * @param time
  * @param state
  */
case class StateSpace(time: Time, state: State) {
  override def toString = time + "," + state.toString
}


// re-write in terms of drift and diffusion functions, with initial state and euler-maruyama step
object Sde {
  def brownianMotion: SdeParameter => Sde = p => p match {
    case param: BrownianParameter =>
      BrownianMotion(param)
    case _ => throw new Exception(s"Incorrect parameters supplied to Brownianmotion, expected BrownianParameter, received $p")
  }

  def ornsteinUhlenbeck: SdeParameter => Sde = p => p match {
    case param: OrnsteinParameter =>
      OrnsteinUhlenbeck(param)
    case _ => throw new Exception(s"Incorrect parameters supplied to Brownianmotion, expected BrownianParameter, received $p")
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

      override def stepFunction(dt: TimeIncrement)(s: State) = s match {
        case Branch(l, r) => for {
          ls <- sde1.stepFunction(dt)(l)
          rs <- sde2.stepFunction(dt)(r)
        } yield Tree.branch(ls, rs)
        case _ => throw new Exception
      }

      def dimension: Int = sde1.dimension + sde2.dimension

      override def dW(dt: TimeIncrement): Rand[Tree[DenseVector[Double]]] = {
        for {
          l <- sde1.dW(dt)
          r <- Applicative[Rand].replicateA(sde2.dimension, Gaussian(0.0, sqrt(dt))).map(x => (DenseVector(x.toArray)))
        } yield l |+ Tree.leaf(r)
      }
    }
  }

  // /**
  //   * Steps all the states using the identity
  //   * @param p a Parameter
  //   * @return a function from state, dt => State
  //   */
  // def stepNull: StepFunction = { p =>
  //   (s, dt) => always(s map (x => x))
  // }

  // /**
  //   * A step function for generalised brownian motion, dx_t = mu dt + sigma dW_t
  //   * @param p an sde parameter
  //   * @return A function from state, time increment to state
  //   */
  // def stepBrownian: StepFunction = { p => (s, dt) => p match {
  //   case BrownianParameter(mu, sigma) =>
  //     always(
  //       s map (x => Gaussian(x + mu * dt, sigma * math.sqrt(dt)).draw)
  //     )
  //   case _ =>
  //     throw new Exception("Incorrect parameters supplied to stepBrownian, expected BrownianParameter")
  //   }
  // }

  // /**
  //   * Steps the state by the value of the parameter "a" 
  //   * multiplied by the time increment "dt"
  //   * @param p a parameter Map
  //   * @return a function from (State, dt) => State, with the
  //   * states being the same structure before and after
  //   */
  // def stepConstant: StepFunction = { p => (s, dt) => p match {
  //     case StepConstantParameter(a) => always(s map (_ + (a * dt)))
  //     case _ => throw new Exception("Incorrect Parameters supplied to stepConstant, expected StepConstantParameter")
  //   }
  // }

  // /**
  //   * A step function for the Ornstein Uhlenbeck process 
  //   * dX_t = alpha(theta - x_t) dt + sigma dW_t
  //   * @param p the parameters of the ornstein uhlenbeck process, theta, alpha and sigma
  //   */
}
