package com.github.jonnylaw.model

import breeze.linalg._
import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
import breeze.numerics.exp
import cats._
import cats.implicits._
import scala.util.{Try, Success, Failure}

sealed trait Parameters {
  def compose(that: Parameters): Parameters = {
    Parameters.branchParameter(this, that)
  }

  def sum(that: Parameters): Try[Parameters]

  def perturb(delta: Double): Rand[Parameters]

  def perturbIndep(delta: Array[Double]): Rand[Parameters]
}
case class LeafParameter(
  initParams: StateParameter,
  scale: Option[Double],
  sdeParam: SdeParameter) extends Parameters {

  def sum(that: Parameters): Try[Parameters] = that match {
    case LeafParameter(init, other_scale, sde) =>
      for {
        init_sum <- initParams sum init
        sde_sum <- sdeParam sum sde
        scale_sum = scale flatMap (v1 => other_scale map (v2 => v1 + v2))
      } yield Parameters.leafParameter(init_sum, scale_sum, sde_sum)
    case _ => Failure(throw new Exception(s"Can't sum LeafParameter and $that"))
  }


  def perturb(delta: Double): Rand[Parameters] = {
    for {
      init <- initParams.perturb(delta)
      sde <- sdeParam.perturb(delta)
      innov <- Gaussian(0.0, delta)
      v = scale map (_ * exp(innov))
    } yield Parameters.leafParameter(init, v, sde)
  }

  def perturbIndep(delta: Array[Double]): Rand[Parameters] = ???
}

case class BranchParameter(left: Parameters, right: Parameters) extends Parameters {

  def sum(that: Parameters): Try[Parameters] = that match {
    case BranchParameter(l, r) => 
      for {
        sum_left <- left sum l
        sum_right <- right sum r
      } yield Parameters.branchParameter(sum_left, sum_right)
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

object Parameters {
  def leafParameter(
    initParams: StateParameter,
    scale: Option[Double],
    sdeParam: SdeParameter): Parameters = {

    LeafParameter(initParams, scale, sdeParam)
  }

  def branchParameter(lp: Parameters, rp: Parameters): Parameters = {
    BranchParameter(lp, rp)
  }

  /**
    * A monoid to compose parameter values
    */
  implicit def composeParameterSemigroup = new Semigroup[Parameters] {
    def combine(lp: Parameters, rp: Parameters): Parameters = lp compose rp
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


