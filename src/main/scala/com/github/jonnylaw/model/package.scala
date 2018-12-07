package com.github.jonnylaw

import breeze.stats.distributions.{Rand, Process}
import breeze.stats.distributions.Rand._
import breeze.linalg.DenseVector
import cats.{Show, Monad}
import cats.implicits._
import cats.data.{Reader, Kleisli, StateT}
import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag
import scala.language.higherKinds
import scala.collection.immutable.TreeMap
import scala.concurrent.Future
import spire.algebra._

package object model {
  type Observation = Double
  type Eta = Double
  type Gamma = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type UnparamModel = Reader[Parameters, Model]
  type UnparamSde = Reader[SdeParameter, Sde]
  type Parameters = Tree[ParamNode]
  type Error[A] = Either[Throwable, A]
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

  // various shows for printing nicely
  implicit val stateShow = new Show[State] {
    def show(a: State): String = a match {
      case Branch(l, r) => show(l) + ", " + show(r)
      case Leaf(x)      => x.data.mkString(", ")
      case Empty        => ""
    }
  }

  implicit def dataShow(implicit S: Show[State]) = new Show[Data] {
    def show(a: Data): String = a match {
      case TimedObservation(t, y) => s"$t, ${y.getOrElse("NA")}"
      case ObservationWithState(t, y, e, g, x) => s"$t, ${y.getOrElse("NA")}, $e, $g, ${S.show(x)}"
      case TimestampObservation(time, t, obs) => s"$time, ${obs.getOrElse("NA")}"
    }
  }

  implicit def decompShow = new Show[DecomposedModel] {
    def show(a: DecomposedModel): String = s"${a.time}, ${a.observation}, ${a.eta}, ${a.gamma}, ${a.state.mkString(", ")}"
  }

  implicit def dvShow = new Show[DenseVector[Double]] {
    def show(dv: DenseVector[Double]) = dv.data.mkString(", ")
  }

  implicit def sdeParamShow(implicit S: Show[DenseVector[Double]]) = new Show[SdeParameter] {
    def show(p: SdeParameter): String = p match {
      case GenBrownianParameter(m0, c0, mu, sigma) =>
        s"""${S.show(m0)}, ${S.show(c0)}, ${S.show(mu)}, ${S.show(sigma)}"""
      case BrownianParameter(m0, c0, sigma) =>
        s"""${S.show(m0)}, ${S.show(c0)}, ${S.show(sigma)}"""
      case OuParameter(m, c, a, s, t) => s"${S.show(m)}, ${S.show(c)}, ${S.show(a)}, ${S.show(s)}, ${S.show(t)}"
    }
  }

  implicit def parameterShow(implicit S: Show[SdeParameter]) = new Show[Parameters] {
    def show(p: Parameters): String = p match {
      case Leaf(ParamNode(v, sde)) => v.map(x => s"$x, ").getOrElse("") + S.show(sde)
      case Branch(l, r) => show(l) + ", " + show(r)
      case Empty => ""
    }
  }

  implicit def stateSpaceShow(implicit S: Show[State]) = new Show[StateSpace[State]] {
    def show(a: StateSpace[State]): String = s"${a.time}, ${S.show(a.state)}"
  }

  implicit def itersShow(implicit S: Show[Parameters], T: Show[StateSpace[State]]) = new Show[MetropState[Parameters, State]] {
    def show(a: MetropState[Parameters, State]): String =
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def paramStateShow(implicit S: Show[Parameters]) = new Show[ParamsState[Parameters]] {
    def show(a: ParamsState[Parameters]): String = 
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def filterShow(implicit S: Show[State]) = new Show[PfState[State]] {
    def show(a: PfState[State]): String = a.observation match {
      case Some(y) => s"${a.t}, $y, ${a.particles.map(S.show).mkString(", ")}, ${a.ess}"
      case None => s"${a.t}, NA, ${a.particles.map(S.show).mkString(", ")}, ${a.ess}"
    }
  }

  implicit def credibleIntervalsShow = new Show[CredibleInterval] {
    def show(a: CredibleInterval): String = s"${a.lower}, ${a.upper}"
  }

  implicit def filterOutShow(implicit S: Show[State], C: Show[CredibleInterval]) = new Show[PfOut[State]] {
    def show(a: PfOut[State]): String = a.observation match {
      case Some(x) =>
        s"${a.time}, $x, ${a.eta}, ${C.show(a.etaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
      case None =>
        s"${a.time}, NA, ${a.eta}, ${C.show(a.etaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
    }
  }

  implicit def forecastOutShow(implicit S: Show[State]) = new Show[ForecastOut[State]] {
    def show(a: ForecastOut[State]): String = s"${a.t}, ${a.obs}, ${a.obsIntervals.toString}, ${a.eta}, ${a.etaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
  }
}
