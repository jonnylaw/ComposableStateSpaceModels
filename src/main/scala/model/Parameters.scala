package model

import breeze.linalg.{DenseMatrix, DenseVector, diag}

sealed trait Parameters {
  import Parameters._

  override def toString = flatten(this).mkString(", ")

  /**
    * Simple function for adding parameters
    * @param that Another set of parameters
    * @return A set of nested (from the left) combined parameters
    */
  def |+|(that: Parameters): Parameters =
    op(this, that)
}

case class LeafParameter(initParams: StateParameter, scale: Option[Double], sdeParam: SdeParameter) extends Parameters
case class BranchParameter(left: Parameters, right: Parameters) extends Parameters

object Parameters {
  def op(lp: Parameters, rp: Parameters): Parameters =
    BranchParameter(lp, rp)

  def flattenSde(p: SdeParameter): Vector[Double] = p match {
    case BrownianParameter(m, s) => m ++ s
    case OrnsteinParameter(theta, alpha, sigma) => theta ++ alpha ++ sigma
    case StepConstantParameter(a) => a
  }

  def flattenInit(p: StateParameter): Vector[Double] = p match {
    case GaussianParameter(m, s) => (m.data ++ diag(s).toArray).toVector
  }

  /**
    * Flattens parameters into a Vector of parameters
    * useful for printing
    */
  def flatten(p: Parameters): Vector[Double] = p match {
    case LeafParameter(init, noise, sde) => noise match {
      case Some(v) => flattenInit(init) ++ Vector(v) ++ flattenSde(sde)
      case None => flattenInit(init) ++ flattenSde(sde)
    }
    case BranchParameter(lp, rp) => flatten(lp) ++ flatten(rp)
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

sealed trait StateParameter
case class GaussianParameter(m0: DenseVector[Double], c0: DenseMatrix[Double]) extends StateParameter

object GaussianParameter {
  def apply(m0: Double, c0: Double): GaussianParameter = {
    new GaussianParameter(DenseVector(m0), DenseMatrix(c0))
  }
}

sealed trait SdeParameter
case class BrownianParameter(mu: Vector[Double], sigma: Vector[Double]) extends SdeParameter {
  def toList(p: BrownianParameter) = List(p.mu, p.sigma)
}
object BrownianParameter {
  def apply(mu: Double, sigma: Double): BrownianParameter = {
    new BrownianParameter(Vector(mu), Vector(sigma))
  } 
}
case class OrnsteinParameter(theta: Vector[Double], alpha: Vector[Double], sigma: Vector[Double]) extends SdeParameter
object OrnsteinParameter {
  def apply(theta: Double, alpha: Double, sigma: Double): OrnsteinParameter = {
    new OrnsteinParameter(Vector(theta), Vector(alpha), Vector(sigma))
  }
}
case class StepConstantParameter(a: Vector[Double]) extends SdeParameter

