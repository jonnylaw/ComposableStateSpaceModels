package com.github.jonnylaw.model

import breeze.linalg._
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import breeze.numerics.exp
import cats._
import cats.implicits._
import scala.util.{Try, Success, Failure}

sealed trait Parameters {
  def sum(that: Parameters): Try[Parameters]

  def perturb(delta: Double): Rand[Parameters]

  def perturbIndep(delta: Array[Double]): Rand[Parameters]
}
case class LeafParameter(scale: Option[Double], sdeParam: SdeParameter) extends Parameters {

  def sum(that: Parameters): Try[Parameters] = that match {
    case LeafParameter(otherScale, sde) =>
      for {
        sdeSum <- sdeParam sum sde
        scaleSum = scale flatMap (v1 => otherScale map (v2 => v1 + v2))
      } yield Parameters.leafParameter(scaleSum, sdeSum)
    case _ => Failure(throw new Exception(s"Can't sum LeafParameter and $that"))
  }


  def perturb(delta: Double): Rand[Parameters] = {
    for {
      sde <- sdeParam.perturb(delta)
      innov <- Gaussian(0.0, delta)
      v = scale map (_ * exp(innov))
    } yield Parameters.leafParameter(v, sde)
  }

  def perturbIndep(delta: Array[Double]): Rand[Parameters] = ???
}

case class BranchParameter(left: Parameters, right: Parameters) extends Parameters {
  def sum(that: Parameters): Try[Parameters] = that match {
    case BranchParameter(l, r) => 
      for {
        sumLeft <- left sum l
        sumRight <- right sum r
      } yield Parameters.branchParameter(sumLeft, sumRight)
    case _ => Failure(throw new Exception(s"Can't add BranchParameter and $that"))
  }

  def perturb(delta: Double): Rand[Parameters] = {
    for {
      l <- left.perturb(delta)
      r <- right.perturb(delta)
    } yield Parameters.branchParameter(l, r)
  }

  def perturbIndep(delta: Array[Double]): Rand[Parameters] = ???
}

case object EmptyParameter extends Parameters {
  def perturb(delta: Double): Rand[Parameters] = Rand.always(Parameters.emptyParameter)
  def perturbIndep(delta: Array[Double]): Rand[Parameters] = Rand.always(Parameters.emptyParameter)
  def sum(that: Parameters): Try[Parameters] = Success(Parameters.emptyParameter)
}

object Parameters {
  def leafParameter(scale: Option[Double], sdeParam: SdeParameter): Parameters = {
    LeafParameter(scale, sdeParam)
  }

  def branchParameter(lp: Parameters, rp: Parameters): Parameters = {
    BranchParameter(lp, rp)
  }

  def emptyParameter: Parameters = EmptyParameter

  /**
    * A monoid to compose parameter values
    */
  implicit def composeParameterMonoid = new Monoid[Parameters] {
    def combine(lp: Parameters, rp: Parameters): Parameters = (lp, rp) match {
      case (EmptyParameter, r) => r
      case (l, EmptyParameter) => l
      case _ => Parameters.branchParameter(lp, rp)
    }

    def empty: Parameters = Parameters.emptyParameter
  }

  /**
    * A sum parameter values
    */
  def sum_parameters(lp: Parameters, rp: Parameters): Try[Parameters] = lp sum rp

  def proposeIdent: Parameters => Rand[Parameters] = p => new Rand[Parameters] {
    def draw = p
  }

  def perturb(delta: Double): Parameters => Rand[Parameters] = p => {
    p.perturb(delta)
  }
}
