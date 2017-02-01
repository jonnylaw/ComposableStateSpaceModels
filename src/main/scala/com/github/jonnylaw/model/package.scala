package com.github.jonnylaw

import breeze.stats.distributions.{Rand, Process}
import fs2.Stream
import breeze.stats.distributions.Rand._
import breeze.linalg.{DenseVector, diag}
import cats.{Monad, Monoid, Show}
import cats.data.Reader

package object model {
  type Observation = Double
  type Eta = Seq[Double]
  type Gamma = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type UnparamModel = Reader[Parameters, Model]
  type UnparamSde = Reader[SdeParameter, Sde]
  type StepFunction = (SdeParameter) => (State, TimeIncrement) => Rand[State]
  type State = Tree[DenseVector[Double]]
  type Resample[A] = (Seq[A], Seq[LogLikelihood]) => Seq[A]

  implicit def randMonad = new Monad[Rand] {
    def pure[A](x: A): Rand[A] = always(x)
    def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Rand[Either[A,B]]): Rand[B] = f(a).draw match {
      case Right(b) => always(b)
      case Left(b) => tailRecM(b)(f)
    }
  }


  implicit def numericDenseVector = new Numeric[DenseVector[Double]] {
    def fromInt(x: Int): DenseVector[Double] = DenseVector(x.toDouble)
    def minus(x: DenseVector[Double],y: DenseVector[Double]): DenseVector[Double] = x - y
    def negate(x: DenseVector[Double]): DenseVector[Double] = - x
    def plus(x: DenseVector[Double],y: DenseVector[Double]): DenseVector[Double] = x + y
    def times(x: DenseVector[Double],y: DenseVector[Double]): DenseVector[Double] = ???
    def toDouble(x: DenseVector[Double]): Double = ???
    def toFloat(x: DenseVector[Double]): Float = ???
    def toInt(x: DenseVector[Double]): Int = ???
    def toLong(x: DenseVector[Double]): Long = ???
    def compare(x: DenseVector[Double],y: DenseVector[Double]): Int = ???
  }

  implicit val stateShow = new Show[State] {
    def show(a: State): String = a match {
      case Branch(l, r) => show(l) + ", " + show(r)
      case Leaf(x) => x.data.mkString(", ")
    }
  }

  implicit def dataShow(implicit S: Show[State]) = new Show[Data] {
    def show(a: Data): String = a match {
      case TimedObservation(t, y) => s"$t, $y"
      case ObservationWithState(t, y, e, g, x) => s"$t, $y, ${e.mkString(", ")}, $g, ${S.show(x)}"
    }
  }

  implicit def sdeParamShow = new Show[SdeParameter] {
    def show(p: SdeParameter): String = p match {
      case BrownianParameter(m0, c0, mu, sigma) =>
        s"""${m0.data.mkString(", ")}, ${diag(m0).data.mkString(", ")}, ${mu.data.mkString(", ")}, ${diag(sigma).data.mkString(", ")}"""
      case OrnsteinParameter(m0, c0, theta, alpha, sigma) =>
        s"""${m0.data.mkString(", ")}, ${diag(m0).data.mkString(", ")}, ${theta.data.mkString(", ")}, ${alpha.data.mkString(", ")}, ${sigma.data.mkString(", ")}"""
    }
  }

  implicit def parameterShow(implicit S: Show[SdeParameter]) = new Show[Parameters] {
    def show(p: Parameters): String = p match {
      case LeafParameter(v, sde) => v.map(x => s"$x, ").getOrElse("") + S.show(sde)
      case BranchParameter(l, r) => show(l) + ", " + show(r)
      case EmptyParameter => ""
    }
  }

  implicit def itersShow(implicit S: Show[Parameters]) = new Show[MetropState] {
    def show(a: MetropState): String = 
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def filterShow = new Show[PfState] {
    def show(a: PfState): String = a.observation match {
      case Some(y) => s"${a.t}, $y, ${ParticleFilter.weightedMean(a.particles, a.weights).flatten.mkString(", ")}"
      case None => s"${a.t}, ${ParticleFilter.weightedMean(a.particles, a.weights).flatten.mkString(", ")}"
    }
  }

  implicit def filterOutShow(implicit S: Show[State]) = new Show[PfOut] {
    def show(a: PfOut): String = a.observation match {
      case Some(x) =>
        s"${a.time}, $x, ${a.eta}, ${a.etaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
      case None =>
        s"${a.time}, NA, ${a.eta}, ${a.etaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
    }
  }

  implicit def forecastOutShow(implicit S: Show[State]) = new Show[ForecastOut] {
    def show(a: ForecastOut): String = s"${a.t}, ${a.obs}, ${a.obsIntervals.toString}, ${a.eta}, ${a.etaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
  }


  implicit def fromProcess[F[_], A](iter: Process[A]): Stream[F, A] = {
    Stream.unfold(iter.step){ case (a, p) => Some((a, p.step)) }
  }
}
