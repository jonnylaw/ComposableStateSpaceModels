package com.github.jonnylaw.model

import DataTypes._
import StateSpace._
import breeze.stats.distributions.{Rand, Density}
import breeze.linalg.DenseVector
import cats._
import cats.implicits._
import breeze.numerics.{cos, sin, sqrt, exp, log}
import breeze.stats.distributions._
import breeze.linalg.{DenseMatrix, DenseVector}

import akka.stream._
import scaladsl._
import akka.NotUsed

/**
  * A description containing the modelled quantities and observations
  * @param sdeState = x_t = p(x_t | x_t-1), the latent state (Optional)
  * @param gamma = f(x_t), the latent state transformed by the linear transformation (Optional)
  * @param eta = g(gamma), the latent state transformed by the linking-function (Optional)
  * @param observation = pi(eta), the observation
  * @param t, the time of the observation
  */
case class Data(
  t: Time,
  observation: Observation,
  eta: Option[Eta],
  zeta: Option[Zeta],
  sdeState: Option[State]) {

  import Data._

  override def toString = {
    if (!sdeState.isEmpty) {
      s"$t, $observation, ${eta.get.head}, ${zeta.get}, " + sdeState.get.flatten.mkString(", ")
    } else {
      t + ", " + observation
    }
  }
}

/**
  * Credible intervals from a set of samples in a distribution
  * @param lower the lower interval
  * @param upper the upper interval
  */
case class CredibleInterval(lower: Double, upper: Double) {
  override def toString = lower + ", " + upper
}


trait Model { self =>
  /**
    * The observation model, a function from eta to a distribution over the observations
    * realisations can be produced from the observation model by calling draw
    */
  def observation: Eta => Rand[Observation]
  /**
    * The linking-function, transforms the state space into the parameter space of the 
    * observation distribution using a possibly non-linear transformation
    */
  def link(x: Zeta): Eta = Vector(x)
  /**
    * The Linear, deterministic transformation function. f is used to add seasonal factors or
    * other time depending linear transformations
    */ 
  def f(s: State, t: Time): Zeta
  /**
    * Distribution over the initial state of the hidden state which realisations 
    * can be simulated from
    */
  def x0: Rand[State]
  /**
    * An exact or approximate solution to a diffusion process, used to advance the latent state.
    * This function returns a distribution over the next state and can be simulated from
    */
  def stepFunction: (State, TimeIncrement) => Rand[State]
  /**
    * The data likelihood, given a fully transformed latent state, eta, and an observation
    * the log-likelihood can be calculated for use in inference algorithms
    */
  def dataLikelihood: (Eta, Observation) => LogLikelihood

  /**
    * Simulate a single step from a model, return a distribution over the possible values
    * of the next step
    * @param deltat the time difference between the previous and next realisation of the process
    * @return a function from the previous datapoint to a Rand (Monadic distribution) representing 
    * the distribution of the next datapoint 
    */
  def simStep(deltat: TimeIncrement): Data => Rand[Data] = d => self match {
    case _: LogGaussianCox => throw new Exception("Can't simulate a step from the LGCP in this way")
    case _ =>
    for {
      x1 <- stepFunction(d.sdeState.get, deltat)
      gamma = f(x1, d.t + deltat)
      eta = link(gamma)
      y1 <- observation(eta)
    } yield Data(d.t + deltat, y1, Some(eta), Some(gamma), Some(x1))
  }

  /**
    * Simulate from a POMP model on an irregular grid, given an initial time and a stream of times 
    * at which simulate from the model
    * @param t0 the start time of the process
    * @return an Akka Stream Flow, from Time to Data 
    */
  def simPompModel(t0: Time): Flow[Time, Data, NotUsed] = {
    val init = for {
      x0 <- x0
      gamma = f(x0, t0)
      eta = link(gamma)
      y <- observation(eta)
    } yield Data(t0, y, Some(eta), Some(gamma), Some(x0))

    Flow[Time].scan(init.draw)((d0, t) => simStep(t - d0.t)(d0).draw)
  }

  /**
    * Simulate from a POMP model (not including the Log-Gaussian Cox-Process) 
    * on a regular grid from t = 0 using the MarkovChain from the breeze package
    * @param dt the time increment between sucessive realisations of the POMP model
    * @return a Process, representing a distribution which depends on previous draws
    */
  def simMarkov(dt: TimeIncrement): Process[Data] = {
    val init = for {
      x0 <- x0
      gamma = f(x0, 0.0)
      eta = link(gamma)
      y <- observation(eta)
    } yield Data(0.0, y, Some(eta), Some(gamma), Some(x0))

    MarkovChain(init.draw)(simStep(dt))
  }

  /**
    * Simulate from any model on a regular grid from t = 0 and return an Akka stream of realisations
    * @param dt the time increment between successive realisations of the POMP model
    * @return an Akka Stream containing a realisation of the process
    */
  def simRegular(dt: TimeIncrement): Source[Data, NotUsed] = self match {
    case _: LogGaussianCox => throw new Exception("Not yet implemented")
    case _ =>
      Source.fromIterator(() => simMarkov(dt).steps)
  }
}

object Model {
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
}

trait UnparamModel extends (Parameters => Either[Throwable, Model])

/**
  * Generalised student t model
  * @param stepFun the diffusion process solution to use for this model
  * @return an unparameterised model of the Student-T model, which can be composed with other models
  */
case class studentTModel(stepFun: StepFunction, df: Int) extends UnparamModel {
  def apply(p: Parameters): Either[Throwable, Model] = p match {
  case LeafParameter(stateParam, Some(v), sdeparam) =>
      Right(
        new Model {

          def observation = x => StudentsT(df) map (a => a * v + x.head)

          def f(s: State, t: Time) = s.head

          def x0 = stateParam match {
            case GaussianParameter(m0, c0) =>
              MultivariateGaussian(m0, c0) map (LeafState(_))
            case _ => throw new Exception("Incorrect parameters supplied to initial state distribution of student t model")
          }

          def stepFunction = (x, dt) => stepFun(sdeparam)(x, dt)

          def dataLikelihood = (eta, y) => 1/v * StudentsT(df).logPdf((y - eta.head) / v)
        }
      )
    case _ => Left(throw new Exception("Student T model requires LeafParameter"))
  }
}

  /**
    * A seasonal model
    * @param period the period of the seasonality
    * @param harmonics the number of harmonics to use in the seasonal model
    * @param stepFun a solution to a diffusion process representing the latent state
    */
case class SeasonalModel(period: Int, harmonics: Int, stepFun: StepFunction) extends UnparamModel {
  def apply(p: Parameters) = p match {
    case LeafParameter(init, Some(v), sde) => 
      Right(new Model {

        def observation = x => Gaussian(x.head, v)

        def buildF(harmonics: Int, t: Time): DenseVector[Double] = {
          val frequency = 2 * math.Pi / period
          DenseVector(((1 to harmonics) flatMap (a =>
            Array(cos(frequency * a * t), sin(frequency * a * t)))).toArray)
        }

        def f(s: State, t: Time) = s match { case LeafState(x) => buildF(harmonics, t) dot x }

        def x0 = init match {
          case GaussianParameter(m0, c0) =>
            MultivariateGaussian(m0, c0) map (LeafState(_))
          case _ => throw new Exception("Incorrect parameters supplied to initial state distribution of student t model")
        }

        def stepFunction = (x, dt) => stepFun(sde)(x, dt)

        def dataLikelihood = (eta, y) => Gaussian(eta.head, v).logPdf(y)
      })
    case _ => Left(throw new Exception(s"Seasonal Model requires LeafParameter got $p"))
  }
}

  /**
    * A linear unparameterised model
    * @param stepFun a solution to a diffusion process representing the evolution of the latent state
    * @return an UnparamModel which can be composed with other models
    */
case class LinearModel(stepFun: StepFunction) extends UnparamModel {
  def apply(p: Parameters) = p match {
    case LeafParameter(init, Some(v), sde) =>
      init match {
        case GaussianParameter(m0, c0) => 
          Right(new Model {
            def observation = x => Gaussian(x.head, v)

            def f(s: State, t: Time) = s.head

            def x0 = MultivariateGaussian(m0, c0) map (LeafState(_))

            def stepFunction = (x, dt) => stepFun(sde)(x, dt)

            def dataLikelihood = (eta, y) => Gaussian(eta.head, v).logPdf(y)
          })
        case _ => Left(throw new Exception("Incorrect initial state parameters in linear model"))
      }
    case _ => Left(throw new Exception("LinearModel requires LeafParameter"))
  }
}

object LinearModel {
  // smart constructors used for validation of model creation
  private def unsafeLinearModel(stepFun: StepFunction)(p: LeafParameter): Model = ???

  // create a model from a validated step function
  def linearModel(stepFun: StepFunction)(p: Parameters): Either[Throwable, Model] = p match {
    case LeafParameter(init, Some(v), sde) =>
      init match {
        case GaussianParameter(m0, c0) => 
          Right(unsafeLinearModel(stepFun)(LeafParameter(init, Some(v), sde)))
        case _ => Left(throw new Exception("Incorrect initial state parameters in linear model"))
      }
    case _ => Left(throw new Exception("LinearModel requires LeafParameter"))
  }
}

  /**
    * The Poisson unparameterised model with a one dimensional latent state
  * @param stepFun a solution to a diffusion process representing the evolution of the latent space
  * @return a Poisson UnparamModel which can be composed with other UnparamModels
  */
case class PoissonModel(stepFun: StepFunction) extends UnparamModel {
  def apply(p: Parameters) = p match {
    case LeafParameter(init, _, sde) =>
      Right(new Model {

      def observation = lambda => Poisson(lambda.head) map (_.toDouble): Rand[Double]

      override def link(x: Double) = Vector(exp(x))

      def f(s: State, t: Time) = s.head

        def x0 = init match {
          case GaussianParameter(m0, c0) =>
            MultivariateGaussian(m0, c0) map (LeafState(_))
          case _ => throw new Exception("Incorrect initial state parameter in poisson model x0")
        }

      def stepFunction = (x, dt) => stepFun(sde)(x, dt)

      def dataLikelihood = (lambda, y) => Poisson(lambda.head).logProbabilityOf(y.toInt)
      })
    case _ => Left(throw new Exception("PoissonModel requires LeafParameter"))
  }
}

  /**
    * The bernoulli model with a one dimensional latent state
    * @param stepFun a solution to a diffusion process 
    */
case class BernoulliModel(stepFun: StepFunction) extends UnparamModel {
  def apply(params: Parameters) = params match {
    case LeafParameter(init, _, sde) =>
      Right(new Model {

      def observation = p => Uniform(0, 1) map (_ < p.head)

      override def link(x: Zeta) = {
        if (x > 6) {
          Vector(1.0)
        } else if (x < -6) {
          Vector(0.0)
        } else {
          Vector(1.0/(1 + exp(-x)))
        }
      }

      def f(s: State, t: Time) = s.head

      def x0 = init match {
        case GaussianParameter(m0, c0) =>
          MultivariateGaussian(m0, c0) map (LeafState(_))
        case _ => throw new Exception("Incorrect initial state parameter in poisson model x0")
      }

      def stepFunction = (x, dt) => stepFun(sde)(x, dt)

      def dataLikelihood = (p, y) => {
        if (y) {
          if (p.head == 0.0) -1e99 else log(p.head)
        } else {
          if (p.head == 1.0) -1e99 else log(1-p.head)
        }
      }

      })
    case _ => Left(throw new Exception("BernoulliModel requires LeafParameter"))
  }
}

  /**
    * The Log-Gaussian Cox-Process is used to model time to event data with 
    * log-gaussian varying hazard rate
    */
case class LogGaussianCox(stepFun: StepFunction) extends UnparamModel {
  def apply(p: Parameters) = p match {
    case LeafParameter(init, _, sde) =>
      Right(new Model {

      def observation = ???

      def f(s: State, t: Time) = s.head

        def x0 = init match {
          case GaussianParameter(m0, c0) =>
            MultivariateGaussian(m0, c0) map (LeafState(_))
        }


      def stepFunction = (x, dt) => stepFun(sde)(x, dt)

      def dataLikelihood = (lambda, y) => lambda.head - lambda(1)
      })
    case _ => Left(throw new Exception("LogGaussianCox requires LeafParameter"))
  }
}

object UnparamModel {
  /**
    * Models form a semigroup, they can be combined to form a composed model
    */
  implicit def modelSemigroup = new Semigroup[UnparamModel] {
    override def combine(m1: UnparamModel, m2: UnparamModel): UnparamModel =
      UnparamModel.op(m1, m2)
  }

  /**
    * Combine two unparameterised models, usually applied with infix notation |+|
    * by importing cats.implicits._, this is not commutative, the observation distribution must 
    * be on the left-hand side of the composition
    * @param mod1 the left-hand model in the composition, if this is a composition of two
    * then the model with the desired observation distribution must be mod1
    * @param mod2 the right-hand model in the composition
    * @return a composed model of mod1 and mod2, which can be composed again
    */
  def op(mod1: UnparamModel, mod2: UnparamModel): UnparamModel = new UnparamModel {
    def apply(p: Parameters): Either[Throwable, Model] = p match {
      case BranchParameter(lp, rp) =>
        for {
          left_mod <- mod1(lp)
          right_mod <- mod2(rp)
        } yield new Model {

          def observation = x => left_mod.observation(x)

          override def link(x: Double) = left_mod.link(x)

          def f(s: State, t: Time) = s match {
            case BranchState(ls, rs) =>
              left_mod.f(ls, t) + right_mod.f(rs, t)
            case x: LeafState =>
              left_mod.f(x, t)
          }

          def x0 =
            for {
              l <- left_mod.x0
              r <- right_mod.x0
            } yield l |+| r

          def stepFunction = (s, dt) => s match {
            case BranchState(ls, rs) =>
              for {
                l <- left_mod.stepFunction(ls, dt)
                r <- right_mod.stepFunction(rs, dt)
              } yield l |+| r
            case x: LeafState =>
              left_mod.stepFunction(x, dt)
          }

          def dataLikelihood = (s, y) => left_mod.dataLikelihood(s, y)
        }
      case _ => Left(throw new Exception(s"Expected BranchParameter to composed model, got $p"))
    }
  }
}
