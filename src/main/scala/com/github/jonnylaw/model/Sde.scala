package com.github.jonnylaw.model

import breeze.stats.distributions._
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import breeze.numerics.{sqrt, exp}
import cats.{Semigroup, Applicative, Eq}
import cats.data.ReaderT
import cats.implicits._
import akka.stream.scaladsl._
import scala.util.{Success, Failure}

trait Sde { self =>
  implicit val rand: RandBasis = Rand
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
  def dW(dt: TimeIncrement): Rand[State] = {
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

  def simInitStream(t0: Time, initialState: State, dt: TimeIncrement): Iterator[StateSpace[State]] = {
    simInit(t0, initialState, dt).steps
  }
}

private final case class GenBrownianMotion(p: GenBrownianParameter, dimension: Int) extends Sde {
  val params: GenBrownianParameter =
    (p.map(Sde.buildParamRepeat(dimension)): @unchecked) match {
      case GenBrownianParameter(m, c, mu, s) => GenBrownianParameter(m, c.map(exp(_)), mu, s.map(exp(_)))
    }

  def initialState: Rand[State] = new Rand[State] {
    def draw = {
      val root = diag(params.c0.map(sqrt(_))) // calculate cholesky of diagonal matrix
      Tree.leaf(root * DenseVector.rand(dimension, rand.gaussian(0, 1)) +:+ params.m0)
    }
  }

  def drift(state: State) = Tree.leaf(Sde.buildParamRepeat(dimension)(p.mu))

  def diffusion(state: State) = Tree.leaf(diag(params.sigma))

  override def stepFunction(dt: TimeIncrement)(s: State) = {
    val res = s map { x =>
      val mean = x + params.mu * dt
      val varianceMatrix = DenseMatrix.eye[Double](dimension) * sqrt(params.sigma * dt)

      varianceMatrix * DenseVector.rand(dimension, rand.gaussian(0, 1)) + mean
    }

    Rand.always(res)
  }
}

private final case class BrownianMotion(p: BrownianParameter, dimension: Int) extends Sde {
  val params: BrownianParameter =
    (p.map(Sde.buildParamRepeat(dimension)): @unchecked) match {
      case BrownianParameter(m, c, s) => BrownianParameter(m, c.map(exp(_)), s.map(exp(_)))
    }

  def initialState: Rand[State] = {
    val root = diag(params.c0.map(sqrt(_))) // calculate cholesky of diagonal matrix
    val res = Tree.leaf(root * DenseVector.rand(dimension, rand.gaussian(0, 1)) +:+ params.m0)
    Rand.always(res)
  }

  def drift(state: State) = Tree.leaf(DenseVector.fill(dimension)(1.0))

  def diffusion(state: State) = Tree.leaf(diag(params.sigma))

  override def stepFunction(dt: TimeIncrement)(s: State) = {
    val res = s map { x =>
      val mean = x
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
  /**
    * Transform the parameters to be constrained
    */
  val params: OuParameter =
    (p.map(Sde.buildParamRepeat(dimension)): @unchecked) match {
      case OuParameter(m, c, ph, mu, s) =>
        OuParameter(m, c.map(exp(_)), ph.map(SdeParameter.logistic), mu, s.map(exp(_)))
    }

  def variance(dt: TimeIncrement) =
    (params.sigma *:* params.sigma /:/ (params.phi *:* 2.0)) *:* (DenseVector.ones[Double](dimension) - exp(params.phi *:* -2.0 * dt))

  override def stepFunction(dt: TimeIncrement)(s: State) = {
    val res: State = s map { x =>
      val mean =  params.mu + (x - params.mu) * exp(- params.phi * dt)

      diag(variance(dt).map(sqrt(_))) * DenseVector.rand(dimension, rand.gaussian(0, 1)) += mean
    }

    Rand.always(res)
  }

  def initialState: Rand[State] = {
    val root = diag(params.c0.map(sqrt(_))) // calculate cholesky of diagonal matrix
    val res = Tree.leaf(root * DenseVector.rand(dimension, rand.gaussian(0, 1)) + params.m0)
    Rand.always(res)
  }

  def drift(state: State) = {
    state map (x => params.phi * (params.mu - x))
  }

  def diffusion(state: State) = Tree.leaf(diag(params.sigma))
}

/**
  * A realisation from a stochastic differential equation
  * @param time the time of the associated state
  * @param state any type
  */
case class StateSpace[S](time: Time, state: S)

object Sde {
  /**
    * Given a target dimension and a vector of values, cyclically repeat the vector of values
    * until the target dimension is reached
    */
  def buildParamRepeat(dim: Int)(m: DenseVector[Double]): DenseVector[Double] = {
    DenseVector.tabulate(dim)(i => m(i % m.size))
  }

  def genBrownianMotion(dimension: Int): UnparamSde = ReaderT { p => p match {
    case param: GenBrownianParameter => Success(GenBrownianMotion(param, dimension))
    case _ => Failure(throw new Exception(s"Incorrect parameters supplied to GenBrownianmotion, expected GenBrownianParameter, received $p"))
  }}

  def brownianMotion(dimension: Int): UnparamSde = ReaderT { p => p match {
    case param: BrownianParameter => Success(BrownianMotion(param, dimension))
    case _ => Failure(throw new Exception(s"Incorrect parameters supplied to Brownianmotion, expected BrownianParameter, received $p"))
  }}

  /**
    * The Ornstein Uhlenbeck Process with mean theta, mean reverting parameter alpha > 0 and diffusion sigma > 0
    * The dimension is controlled by an explicit dimension parameter, the parameters, theta, alpha and sigma are recycled
    * for each dimension required
    * The parameters should be specified on a log scale.
    * @param dimension the dimension of the diffusion process
    * @return a function from SdeParameter => Sde
    */
  def ouProcess(dimension: Int): UnparamSde = ReaderT { p => p match {
    case param: OuParameter => Success(OuProcess(param, dimension))
    case _ => Failure(throw new Exception(s"Incorrect parameters supplied to OuProcess, expected OuParameter, received $p"))
  }}

  implicit def sdeSemigroup = new Semigroup[Sde] {
    def combine(sde1: Sde, sde2: Sde): Sde = new Sde {
      def initialState: Rand[State] = for {
        l <- sde1.initialState
        r <- sde2.initialState
      } yield Tree.branch(l, r)

      def drift(state: State): Tree[DenseVector[Double]] = state match {
        case Branch(l, r) => Tree.branch(sde1.drift(l), sde2.drift(r))
        case _ =>
          throw new Exception("Can't apply a composed SDE to a non-composed state")
      }

      def diffusion(state: State): Tree[DenseMatrix[Double]] = state match {
        case Branch(l, r) => Tree.branch(sde1.diffusion(l), sde2.diffusion(r))
        case _ =>
          throw new Exception("Can't apply a composed SDE to a non-composed state")
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
        } yield l +++ Tree.leaf(r)
      }
    }
  }

  /**
    * Eq Type class for DenseVectors
    */
  implicit def eqDenseVec =
    new Eq[DenseVector[Double]] {
      def tolerantEquiv(tol: Int)(a: Double, b: Double): Boolean = {
        Math.abs(a - b) < Math.pow(10, -tol)
      }
      def eqv(x: DenseVector[Double], y: DenseVector[Double]): Boolean = {
        (x.data.zip(y.data)).
          map { case (a, b) => tolerantEquiv(3)(a, b) }.
          reduce(_ && _)
        }
      }
}
