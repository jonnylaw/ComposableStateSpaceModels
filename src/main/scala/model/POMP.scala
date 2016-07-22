package model

import breeze.numerics.{cos, sin, sqrt, exp, log}
import breeze.stats.distributions.{Bernoulli, Poisson, Gaussian, Rand, MultivariateGaussian, StudentsT, NegativeBinomial, Density}
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

  implicit def bool2obs(b: Boolean): Observation = if (b) 1.0 else 0.0
  implicit def obs2bool(o: Observation): Boolean = if (o == 0.0) false else true

  def studentTModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State], df: Int): Parameters => Model = p => new Model {

    def observation = x => p match {
          case LeafParameter(_,v,_) => v match {
            case Some(scale) => new Rand[Observation] with Density[Observation] {
              def draw = StudentsT(df).draw*scale + x.head
              def apply(y: Observation) = 1/scale * StudentsT(df).logPdf((y - x.head) / scale)
            }
          }
    }

    def f(s: State, t: Time) = s.head

    def x0 = p match {
        case LeafParameter(stateParam, _, _) =>
          stateParam match {
            case GaussianParameter(m0, c0) =>
              MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
          }
    }

    def stepFunction = (x, dt) => p match {
      case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

    def dataLikelihood = (s, y) =>
        p match {
          case LeafParameter(_,v,_  @unchecked) => v match {
            case Some(scale  @unchecked) => 1/scale * StudentsT(df).logPdf((y - s.head) / scale)
          }
        }
  }

  def SeasonalModel(
    period: Int,
    harmonics: Int,
    stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = x => p match {
      case LeafParameter(_,v,_) => v match {
        case Some(noisesd) => Gaussian(x.head, noisesd)
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
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

    def dataLikelihood = (s, y) =>
        p match {
          case LeafParameter(_,v,_  @unchecked) => v match {
            case Some(noisesd  @unchecked) => Gaussian(s.head, noisesd).logPdf(y)
              }
        }
  }

  def LinearModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = x => p match {
          case LeafParameter(_,v,_  @unchecked) => 
            v.map(Gaussian(x.head, _)).get
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
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

    def dataLikelihood = (s, y) => p match {
      case LeafParameter(_,v,_) => v match {
        case Some(noisesd @unchecked) => Gaussian(s.head, noisesd).logPdf(y)
      }
    }
  }

  def PoissonModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = lambda => new Rand[Observation] with Density[Observation] {
      def draw = Poisson(lambda.head).draw
      def apply(y: Observation) = Poisson(lambda.head).logProbabilityOf(y.toInt)
    }

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
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

    def dataLikelihood = (s, y) =>
      (Poisson(s.head).logProbabilityOf(y.toInt))
  }

  def BernoulliModel(stepFun: (SdeParameter) => (State, TimeIncrement) => Rand[State]): Parameters => Model = params => new Model {

    def observation = p => new Rand[Observation] with Density[Observation] {
      def draw = {
        scala.util.Random.nextDouble < p.head
      }
      def apply(y: Observation) = {
        if (y) {
          if (p.head == 0.0) -1e99 else log(p.head)
        } else {
          if ((1 - p.head) == 0.0) -1e99 else log(1-p.head)
        }
      }
    }

    override def link(x: Gamma) = {
      if (x > 6) {
        Vector(1.0)
      } else if (x < -6) {
        Vector(0.0)
      } else {
        Vector(1.0/(1 + exp(-x)))
      }
    }

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
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

    /**
      * log(0) is undefined, so we just return a very small number for the log-likelihood when the probability is one
      */
    def dataLikelihood = {
      (p, y) =>
      if (y) {
        if (p.head == 0.0) -1e99 else log(p.head)
      } else {
        if ((1 - p.head) == 0.0) -1e99 else log(1-p.head)
      }
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
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

     /**
      * The data likelihood requires two parameters, the hazard and cumulative hazard
      */
    def dataLikelihood = (s, y) => s.head - s(1)
  }

  def negativeBinomial(stepFun: SdeParameter => (State, TimeIncrement) => Rand[State]): Parameters => Model = p => new Model {

    def observation = mu => new Rand[Observation] with Density[Observation] {
      def draw = {
        p match {
          case LeafParameter(_, scale, _) =>
            scale.map(NegativeBinomial(mu.head, _).draw).get
        }
      }
      def apply(y: Observation) =  p match {
        case LeafParameter(_, scale, _) =>
          NegativeBinomial(mu.head, scale.get).logProbabilityOf(y.toInt)
        case _ => throw new Exception("Can't determine the likelihood using a branch parameter")
      }
    }

    def f(s: State, t: Time) = s.head

    def x0 = p match {
      case LeafParameter(stateParam, _, _ @unchecked) =>
        stateParam match {
          case GaussianParameter(m0, c0) =>
            MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
        }
      case _ => throw new Exception("State of single model must receive a Leaf Parameter")
    }

    def stepFunction = (x, dt) => p match {
      case LeafParameter(_,_,sdeparam @unchecked) => stepFun(sdeparam)(x, dt)
      case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
    }

    def dataLikelihood = (s, y) => p match {
      case LeafParameter(_, scale, _) =>
        NegativeBinomial(s.head, scale.get).logProbabilityOf(y.toInt)
      case _ => throw new Exception("Can't determine the likelihood using a branch parameter")
    }
  }
}
