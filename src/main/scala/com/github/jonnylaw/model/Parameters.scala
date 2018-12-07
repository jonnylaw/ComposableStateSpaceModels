package com.github.jonnylaw.model

import breeze.linalg.{DenseMatrix, DenseVector, diag, support}
import breeze.linalg.eigSym._
import breeze.stats.covmat
import breeze.stats.distributions._
import breeze.numerics.{exp, sqrt}
import cats.{Applicative, Eq, Traverse}
import cats.implicits._
import spire.algebra.AdditiveSemigroup
import spire.implicits._
import scala.language.higherKinds

case class ParamNode(scale: Option[Double], sdeParam: SdeParameter)

object Parameters {
  /**
    * Constructor for a leaf parameter value
    */
  def apply(scale: Option[Double], sdeParam: SdeParameter): Parameters = {
    Tree.leaf(ParamNode(scale, sdeParam))
  }


  implicit def addSemiParamNode(implicit S: AdditiveSemigroup[SdeParameter]) = new AdditiveSemigroup[ParamNode] {
    def plus(x: ParamNode, y: ParamNode) = {
      val newScale = for {
        v <- x.scale
        u <- y.scale
      } yield u + v

      ParamNode(newScale, x.sdeParam + y.sdeParam)
    }
  }

  /**
    * Apply a function to each element of the parameter node case class
    */
  def map(fa: ParamNode)(f: Double => Double): ParamNode = {
    ParamNode(fa.scale map f, fa.sdeParam map (_ mapValues f))
  }

  def traverseRand(fa: ParamNode)(f: Double => Rand[Double]): Rand[ParamNode] = {
    Applicative[Rand].map2(fa.scale traverse f, fa.sdeParam.traverse(f))(ParamNode(_, _))
  }

  def traverse[F[_]: Applicative](fa: ParamNode)(f: Double => F[Double]): F[ParamNode] = {
    Applicative[F].map2(fa.scale traverse f, fa.sdeParam.traverse(f))(ParamNode(_, _))
  }

  /**
    * Calculate the mean of the parameter values
    */
  def mean(params: Seq[Parameters])(implicit S: AdditiveSemigroup[ParamNode]): Parameters = {
    val sum = params.reduce(_ + _)

    sum.map(x => map(x)(_/params.length))
  }

  def proposeIdent: Parameters => Rand[Parameters] = p => Rand.always(p)

  /**
    * Perturb a parameter tree with independent Gaussian noise, with variance delta
    */
  def perturb(delta: Double): Parameters => Rand[Parameters] = p => {
    p.traverse((y: ParamNode) => traverseRand(y)(Gaussian(_, sqrt(delta))))
  }

  /**
    * Can add a dense vector to a param node
    */
  implicit def addableParamNode(implicit S: Addable[SdeParameter]) = new Addable[ParamNode] {
    def add(fa: ParamNode, that: DenseVector[Double]): ParamNode = fa.scale match {
      case Some(v) => ParamNode(Some(v + that(0)), S.add(fa.sdeParam, that(1 to -1)))
      case None    => ParamNode(None, S.add(fa.sdeParam, that))
    }
  }

  /**
    * Returns the size of a parameter tree
    */
  def paramSize(fa: Parameters): Int =
    flattenParams(fa).size

  /**
    * Flatten parameters into a sequence of doubles
    */
  def flattenParams(fa: Parameters): Seq[Double] = fa match {
    case Leaf(p)      => p.scale match {
      case Some(v)  => v +: p.sdeParam.flatten
      case None     => p.sdeParam.flatten
    }
    case Branch(l, r) => flattenParams(l) ++ flattenParams(r)
    case Empty        => Seq()
  }

  implicit def addableParams(implicit S: Addable[ParamNode]) = new Addable[Parameters] {
    def add(fa: Parameters, that: DenseVector[Double]): Parameters = fa match {
      case Leaf(v)      => Tree.leaf(S.add(v, that))
      case Branch(l, r) => add(l, that(0 to paramSize(l) - 1)) |+| add(r, that(paramSize(l) to -1))
      case Empty        => Empty
    }
  }

  implicit def eqParamNode[A](implicit ev: Eq[SdeParameter]): Eq[ParamNode] = new Eq[ParamNode] {
    def eqv(x: ParamNode, y: ParamNode): Boolean = {
      x.scale === y.scale && x.sdeParam === y.sdeParam
    }
  }

  def perturbMvn(chol: DenseMatrix[Double])(implicit rand: RandBasis = Rand, S: Addable[Parameters]) = { (p: Parameters) =>
    val innov = chol * DenseVector.rand(chol.cols, rand.gaussian(0, 1))
    Rand.always(S.add(p, innov))
  }

  def perturbMvnEigen(
    eigen: EigSym[DenseVector[Double], DenseMatrix[Double]])
    (implicit rand: RandBasis = Rand, S: Addable[Parameters]) = { (p: Parameters) =>

    val q = eigen.eigenvectors * diag(eigen.eigenvalues.mapValues(x => sqrt(x)))
    val innov = q * DenseVector.rand(eigen.eigenvalues.length, rand.gaussian(0, 1))
    Rand.always(S.add(p, innov))
  }

  /**
    * Check if two parameter trees are isomorphic in shape when traversed from the left
    */
  def isIsomorphic(p: Parameters, p1: Parameters): Boolean = {
    p.flatten == p1.flatten
  }

  /**
    * Calculate the covariance of a sequence of parameters
    */
  def covariance(samples: Seq[Parameters]): DenseMatrix[Double] = {
    val dim = paramSize(samples.head)
    val m = new DenseMatrix(samples.size, dim, samples.map(p => flattenParams(p).toArray).toArray.transpose.flatten)
    covmat.matrixCovariance(m)
  }

  /**
    * Get parameter names from a tree of parameters
    * @param p a tree of parameters
    * @return a list of strings containing the names of the parameters
    */
  def paramNames(p: Parameters): List[String] = p match {
    case Leaf(ParamNode(scale, sdeParam)) => scale match {
      case Some(v) => "scale" :: sdeParam.names
      case None => sdeParam.names
    }
    case Branch(l, r)                     => paramNames(l) ++ paramNames(r)
    case Empty                            => List[String]()
  }
}
