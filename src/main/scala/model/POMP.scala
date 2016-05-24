package model

import breeze.numerics.{cos, sin, sqrt, exp, log}
import breeze.stats.distributions.{Bernoulli, Poisson, Gaussian, Rand, MultivariateGaussian, ContinuousDistr}
import breeze.linalg.{DenseMatrix, DenseVector}
import scala.language.implicitConversions
import java.io.Serializable

object POMP {
  type Eta = Vector[Double]
  type Gamma = Double
  type Observation = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double

  // these
  implicit def bool2obs(b: Boolean): Observation = if (b) 1.0 else 0.0
  implicit def obs2bool(o: Observation): Boolean = if (o == 0.0) false else true
  implicit def denseVector2Vector(dv: DenseVector[Double]): Vector[Double] = dv.toArray.toVector

  def SeasonalModel(
    period: Int,
    harmonics: Int,
    stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = x => {
      new Rand[Observation] { 
        def draw = p match {
          case LeafParameter(_,v,_) => v match {
            case Some(noisesd) => Gaussian(x.head, noisesd).draw
          }
        }
      }
    }

    def buildF(harmonics: Int, t: Time): DenseVector[Double] = {
      val frequency = 2 * math.Pi / period
      DenseVector(((1 to harmonics) flatMap (a =>
        Array(cos(frequency * a * t), sin(frequency * a * t)))).toArray)
    }   
    
    def f(s: State, t: Time) = s match {
        case LeafState(x @unchecked) => buildF(harmonics, t) dot DenseVector(x.toArray)
    }

    def x0 = p match {
        case LeafParameter(stateParam, _, _) =>
          stateParam match {
            case GaussianParameter(m0, c0) =>
              MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
          }
    }

    def stepFunction = (x, dt) => p match {
      case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
    }

    def dataLikelihood = (s, y) =>
        p match {
          case LeafParameter(_,v,_  @unchecked) => v match {
            case Some(noisesd  @unchecked) => Gaussian(s.head, noisesd).logPdf(y)
              }
        }
  }

  def LinearModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = x => new Rand[Observation] {
      def draw = {
        p match {
          case LeafParameter(_,v,_  @unchecked) => 
            v.map(Gaussian(x.head, _).draw).get
        }
      }
    }

    def f(s: State, t: Time) = s.head

    def x0 = p match {
        case LeafParameter(stateParam, _, _  @unchecked) =>
          stateParam match {
            case GaussianParameter(m0, c0  @unchecked) =>
              MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
          }
    }

    def stepFunction = (x, dt) => p match {
      case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
    }

    def dataLikelihood = (s, y) => p match {
      case LeafParameter(_,v,_) => v match {
        case Some(noisesd @unchecked) => Gaussian(s.head, noisesd).logPdf(y)
      }
    }
  }

  def PoissonModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model with Serializable {

    def observation = lambda => new Rand[Observation] { def draw = Poisson(lambda.head).draw }

    override def link(x: Double) = Vector(exp(x))

    def f(s: State, t: Time) = s.head

    def x0 = p match {
        case LeafParameter(stateParam, _, _  @unchecked) =>
          stateParam match {
            case GaussianParameter(m0, c0) =>
              MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
          }
    }

    def stepFunction = (x, dt) => p match {
      case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
    }

    def dataLikelihood = (s, y) =>
      (Poisson(s.head).logProbabilityOf(y.toInt))
  }

  def BernoulliModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = params => new Model {

    def observation = p => new Rand[Observation] {
      def draw = {
        val bern = new Bernoulli(p.head)
        bern.draw
      }
    }

    override def link(x: Gamma) = Vector(1.0/(1 + exp(-x)))

    def f(s: State, t: Time) = s.head

    def x0 = params match {
        case LeafParameter(stateParam, _, _ @unchecked) =>
          stateParam match {
            case GaussianParameter(m0, c0) =>
              MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
          }
    }

    def stepFunction = (x, dt) => params match {
      case LeafParameter(_,_,sdeparam @unchecked) => stepFun(sdeparam)(x, dt)
    }

    def dataLikelihood = {
      (s, y) =>
        val bern = new Bernoulli(s.head)
        bern.logProbabilityOf(y)
    }
  }

  /**
    * The Log-Gaussian Cox-Process is used to model time to event data with 
    * log-gaussian varying hazard rate
    */
  def LogGaussianCox(
    stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = ???

    def f(s: State, t: Time) = s.head

    def x0 = p match {
        case LeafParameter(stateParam, _, _ @unchecked) =>
          stateParam match {
            case GaussianParameter(m0, c0) =>
              MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
          }
    }

    def stepFunction = (x, dt) => p match {
      case LeafParameter(_,_,sdeparam @unchecked) => stepFun(sdeparam)(x, dt)
    }

     /**
      * The data likelihood requires two parameters, the hazard and cumulative hazard
      */
    def dataLikelihood = (s, y) => s.head - s(1)
  }
}
