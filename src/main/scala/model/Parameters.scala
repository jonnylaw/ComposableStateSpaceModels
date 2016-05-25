package model

import breeze.linalg.{DenseMatrix, DenseVector, diag}
import breeze.stats.distributions.{Rand, Gaussian}
import breeze.numerics.exp

sealed trait Parameters {
  override def toString = Parameters.flatten(this).mkString(", ")
  def |+|(that: Parameters): Parameters =
    Parameters.combine(this, that)
  def length: Int = Parameters.length(this)
  def isEmpty: Boolean = Parameters.isEmpty(this)
  def perturb(delta: Double): Rand[Parameters] =
    Parameters.perturb(delta)(this)
}

case class LeafParameter(initParams: StateParameter, scale: Option[Double], sdeParam: SdeParameter) extends Parameters
case class BranchParameter(left: Parameters, right: Parameters) extends Parameters

object LeafParameter {
  def apply(): LeafParameter = {
    LeafParameter(EmptyParameter, None, EmptyStepParameter)
  }
}

object Parameters {
  def combine(lp: Parameters, rp: Parameters): Parameters =
    if (lp.isEmpty) {
      rp
    } else if (rp.isEmpty) {
      lp
    } else {
      BranchParameter(lp, rp)
    }

  def zero: Parameters = LeafParameter()

  def isEmpty(p: Parameters): Boolean = p match {
    case LeafParameter(p, v, s) => (p, v, s) match {
      case (EmptyParameter, None, EmptyStepParameter) => true
      case _ => false
    }
    case BranchParameter(lp, rp) => isEmpty(lp) && isEmpty(rp)
  }

  def perturb(delta: Double): Parameters => Rand[Parameters] = p => p match {
    case LeafParameter(initParams, v, sdeParams) =>
      for {
        init <- initParams.perturb(delta)
        sde <- sdeParams.perturb(delta)
      } yield LeafParameter(init, v map (x => x * exp(Gaussian(0, delta).draw)), sde)
    case BranchParameter(lp, rp) =>
      for {
        l <- perturb(delta)(lp)
        r <- perturb(delta)(rp)
      } yield BranchParameter(l, r)
  }


  /**
    * Flattens parameters into a Vector of parameters
    * useful for printing
    */
  def flatten(p: Parameters): Vector[Double] = p match {
    case LeafParameter(init, noise, sde) => noise match {
      case Some(v) => init.flatten ++ Vector(v) ++ sde.flatten
      case None => init.flatten ++ sde.flatten
    }
    case BranchParameter(lp, rp) => flatten(lp) ++ flatten(rp)
  }

  def length(p: Parameters): Int = {
    flatten(p).length
  }

  def getSdeParameterNames(p: SdeParameter): IndexedSeq[String] = p match {
    case BrownianParameter(m, s) =>
      val paramSize = (0 to (m.size - 1))
      (paramSize map (i => s"mu$i")) ++ (paramSize map (i => s"sigma$i"))
    case OrnsteinParameter(t, a, s) =>
      val paramSize = (0 to (t.size - 1))
      (paramSize map (i => s"theta$i")) ++ (paramSize map (i => s"alpha$i")) ++ (paramSize map (i => s"sigma$i"))
    case StepConstantParameter(a) => IndexedSeq("a")
  }

  def getInitParamNames(p: StateParameter): IndexedSeq[String] = p match {
    case GaussianParameter(m, s) =>
      val paramSize = (0 to (m.size -1))
      (paramSize map (i => s"m0$i")) ++ (paramSize map (i => s"C0$i"))
  }

  /**
    * Get parameter names using case class names
    * useful for header rows in CSV files etc
    * @param p the parameters to get the names of
    * @return an indexed sequence of strings
    */
  def getParameterNames(p: Parameters): IndexedSeq[String] = p match {
    case LeafParameter(init, noise, sde) => noise match {
      case Some(v) =>
        getInitParamNames(init) ++ Vector("noiseSd") ++ getSdeParameterNames(sde)
      case None => getInitParamNames(init) ++ getSdeParameterNames(sde)
    }
    case BranchParameter(lp, rp) => getParameterNames(lp) ++ getParameterNames(rp)
  }

  /**
    * Transforms a parameter tree to a map of strings to doubles
    * @param p a parameter tree
    * @return a map of parameter names -> parameter value
    */
  def paramsToMap(p: Parameters): Map[String, Double] = {
    (getParameterNames(p), flatten(p)).zipped.map{ case (k, v) => (k -> v) }.toMap
  }
}

sealed trait StateParameter {
  def length: Int = StateParameter.length(this)
  def flatten: Vector[Double] = StateParameter.flatten(this)
  def perturb(delta: Double): Rand[StateParameter]
}
case class GaussianParameter(m0: DenseVector[Double], c0: DenseMatrix[Double]) extends StateParameter {
  def perturb(delta: Double): Rand[GaussianParameter] = {
    new Rand[GaussianParameter] {
      def draw = {
        GaussianParameter(
          m0 map (Gaussian(_, delta).draw),
          diag(diag(c0) map (x => x * exp(Gaussian(0, delta).draw))))
      }
    }
  }
}
case object EmptyParameter extends StateParameter {
   def perturb(delta: Double): Rand[StateParameter] = new Rand[StateParameter] { def draw = ??? }
}

object GaussianParameter {
  def apply(m0: Double, c0: Double): GaussianParameter = {
    new GaussianParameter(DenseVector(m0), DenseMatrix(c0))
  }
}

object StateParameter {
  def flatten(p: StateParameter): Vector[Double] = p match {
    case GaussianParameter(m, s) => (m.data ++ diag(s).toArray).toVector
  }
  def length(p: StateParameter): Int = flatten(p).length
}

sealed trait SdeParameter {
  def length: Int = SdeParameter.length(this)
  def flatten: Vector[Double] = SdeParameter.flatten(this)
  def perturb(delta: Double): Rand[SdeParameter]
}

object SdeParameter {
  def flatten(p: SdeParameter): Vector[Double] = p match {
    case BrownianParameter(m, s) => m.data.toVector ++ diag(s).data.toVector
    case OrnsteinParameter(theta, alpha, sigma) => theta ++ alpha ++ sigma
    case StepConstantParameter(a) => a.data.toVector
 }

  def length(p: SdeParameter): Int = flatten(p).length  
}

case class BrownianParameter(mu: DenseVector[Double], sigma: DenseMatrix[Double]) extends SdeParameter {
  def perturb(delta: Double): Rand[BrownianParameter] = {
    new Rand[BrownianParameter] {
      def draw = {
        BrownianParameter(
          mu map (Gaussian(_, delta).draw),
          diag(diag(sigma) map (x => x * exp(Gaussian(0, delta).draw))))
      }
    }
  }
}

object BrownianParameter {
  def apply(mu: Double, sigma: Double): BrownianParameter = {
    new BrownianParameter(DenseVector(mu), DenseMatrix(sigma))
  }
}
case class OrnsteinParameter(theta: Vector[Double], alpha: Vector[Double], sigma: Vector[Double]) extends SdeParameter {

  def perturb(delta: Double): Rand[OrnsteinParameter] = {
    new Rand[OrnsteinParameter] {
      def draw = {
        // alpha and sigme are non-negative, propose on log-scale
        OrnsteinParameter(
          theta map (Gaussian(_, delta).draw),
          alpha map (x => x * exp(Gaussian(0, delta).draw)),
          sigma map (x => x * exp(Gaussian(0, delta).draw)))
      }
    }
  }
}

object OrnsteinParameter {
  def apply(theta: Double, alpha: Double, sigma: Double): OrnsteinParameter = {
    new OrnsteinParameter(Vector(theta), Vector(alpha), Vector(sigma))
  }
}

case class StepConstantParameter(a: DenseVector[Double]) extends SdeParameter {
  def perturb(delta: Double): Rand[StepConstantParameter] = {
    new Rand[StepConstantParameter] {
      def draw = StepConstantParameter(
        a map (Gaussian(_, delta).draw))
    }
  }
}

case object EmptyStepParameter extends SdeParameter {
   def perturb(delta: Double): Rand[SdeParameter] = new Rand[SdeParameter] { def draw = ??? }
}
