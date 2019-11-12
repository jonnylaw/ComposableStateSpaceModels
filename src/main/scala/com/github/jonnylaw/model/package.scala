package com.github.jonnylaw

import breeze.stats.distributions.Rand
import breeze.stats.distributions.Rand._
import breeze.linalg.DenseVector
import cats.data.{ReaderT, Reader}
import cats.Monad
import spire.algebra._
import scala.util.Try

package object model {
  type Observation = Double
  type Eta = Double
  type Gamma = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type UnparamModel = ReaderT[Try, Parameters, Model]
  type UnparamSde = ReaderT[Try, SdeParameter, Sde]
  type Parameters = Tree[ParamNode]
  type StepFunction = (SdeParameter) => (State, TimeIncrement) => Rand[State]
  type State = Tree[DenseVector[Double]]
  type Resample[A] = (Vector[A], Vector[LogLikelihood]) => Vector[A]
  type BootstrapFilter[P, S] = Reader[P, (LogLikelihood, Vector[S])]

  implicit def randMonad = new Monad[Rand] {
    def pure[A](x: A): Rand[A] = always(x)
    def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Rand[Either[A,B]]): Rand[B] = f(a).draw match {
      case Right(b) => always(b)
      case Left(a1) => tailRecM(a1)(f)
    }
  }

  implicit def groupDenseVector = new AdditiveGroup[DenseVector[Double]] {
    def plus(x: DenseVector[Double], y: DenseVector[Double]) = x + y
    def negate(x: DenseVector[Double]) = -x

    // how can we implement an identity element without first knowing the size of the vector?
    def zero = DenseVector.zeros[Double](10)
  }
}
