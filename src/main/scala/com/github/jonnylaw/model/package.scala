package com.github.jonnylaw

import breeze.stats.distributions.{Rand, Process}
import breeze.stats.distributions.Rand._
import breeze.linalg.DenseVector
import cats._
import cats.implicits._
import cats.data.{Reader, Kleisli, StateT}
import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag
import scala.language.higherKinds
import scala.collection.immutable.TreeMap
import scala.concurrent.Future

package object model {
  type Observation = Double
  type Eta = Double
  type Gamma = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type Error[A] = Either[Throwable, A]
  type UnparamModel = Kleisli[Error, Parameters, Model]
  type UnparamSde = Kleisli[Error, SdeParameter, Sde]
  type StepFunction = (SdeParameter) => (State, TimeIncrement) => Rand[State]
  type State = Tree[DenseVector[Double]]
  type Resample[A] = (Vector[A], Vector[LogLikelihood]) => Vector[A]
  type BootstrapFilter = Kleisli[Error, Parameters, (LogLikelihood, Vector[StateSpace])]

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

  implicit def parVectorCollection = new Collection[ParVector] {
    def pure[A](a: A): ParVector[A] = ParVector(a)
    def isEmpty[A](fa: ParVector[A]): Boolean = fa.isEmpty
    def map[A, B](fa: ParVector[A])(f: A => B): ParVector[B] = fa.map(f)
    def flatMap[A, B](fa: ParVector[A])(f: A => ParVector[B]): ParVector[B] = fa.flatMap(f)
    def get[A](fa: ParVector[A])(i: Int): A = fa(i)
    def indexWhere[A](fa: ParVector[A])(cond: A => Boolean): Int = fa.indexWhere(cond)
    def scanLeft[A, B](fa: ParVector[A],z: B)(f: (B, A) => B): ParVector[B] = fa.scanLeft(z)(f)
    def foldLeft[A, B](fa: ParVector[A],b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    def empty[A]: ParVector[A] = ParVector()
    def append[A](fa: ParVector[A])(a: A): ParVector[A] = a +: fa
    def size[A](fa: ParVector[A]) = fa.size
    def combineK[A](x: ParVector[A], y: ParVector[A]): ParVector[A] = x ++ y
    def indices[A](fa: ParVector[A]) = fa.zipWithIndex.map(_._2)
    def toArray[A: ClassTag](fa: ParVector[A]): Array[A] = fa.toArray
    def fill[A](n: Int)(a: => A): ParVector[A] = ParVector.fill(n)(a)
    def unzip[A, B](fa: ParVector[(A, B)]): (ParVector[A], ParVector[B]) = fa.unzip
    def max[A: Ordering](fa: ParVector[A]): A = fa.max
    def toVector[A](fa: ParVector[A]): Vector[A] = fa.toVector
    def zip[A, B](fa: ParVector[A], fb: ParVector[B]): ParVector[(A, B)] = fa.zip(fb)
    def drop[A](fa: ParVector[A])(n: Int): ParVector[A] = fa.drop(n)
    def toTreeMap[A: Ordering, B](fa: ParVector[(A, B)]): TreeMap[A, B] = 
      TreeMap.empty[A, B] ++ fa
  }

  implicit def vectorCollection = new Collection[Vector] {
    def pure[A](a: A): Vector[A] = Vector(a)
    def isEmpty[A](fa: Vector[A]): Boolean = fa.isEmpty
    def map[A, B](fa: Vector[A])(f: A => B): Vector[B] = fa.map(f)
    def flatMap[A, B](fa: Vector[A])(f: A => Vector[B]): Vector[B] = fa.flatMap(f)
    def get[A](fa: Vector[A])(i: Int): A = fa(i)
    def indexWhere[A](fa: Vector[A])(cond: A => Boolean): Int = fa.indexWhere(cond)
    def scanLeft[A, B](fa: Vector[A],z: B)(f: (B, A) => B): Vector[B] = fa.scanLeft(z)(f)
    def foldLeft[A, B](fa: Vector[A],b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    def empty[A]: Vector[A] = Vector()
    def append[A](fa: Vector[A])(a: A): Vector[A] = a +: fa
    def size[A](fa: Vector[A]) = fa.size
    def combineK[A](x: Vector[A], y: Vector[A]): Vector[A] = x ++ y
    def indices[A](fa: Vector[A]) = fa.zipWithIndex.map(_._2)
    def fill[A](n: Int)(a: => A) = Vector.fill(n)(a)
    def toArray[A: ClassTag](fa: Vector[A]): Array[A] = fa.toArray
    def unzip[A, B](fa: Vector[(A, B)]): (Vector[A], Vector[B]) = fa.unzip
    def max[A: Ordering](fa: Vector[A]): A = fa.max
    def toVector[A](fa: Vector[A]): Vector[A] = fa
    def zip[A, B](fa: Vector[A], fb: Vector[B]): Vector[(A, B)] = fa.zip(fb)
    def drop[A](fa: Vector[A])(n: Int): Vector[A] = fa.drop(n)
    def toTreeMap[A: Ordering, B](fa: Vector[(A, B)]): TreeMap[A, B] = TreeMap.empty[A, B] ++ fa
  }

  // various shows for printing nicely
  implicit val stateShow = new Show[State] {
    def show(a: State): String = a match {
      case Branch(l, r) => show(l) + ", " + show(r)
      case Leaf(x) => x.data.mkString(", ")
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

  implicit def sdeParamShow = new Show[SdeParameter] {
    def show(p: SdeParameter): String = p match {
      case GenBrownianParameter(m0, c0, mu, sigma) =>
        s"""$m0, $c0, $mu, $sigma"""
      case BrownianParameter(m0, c0, sigma) =>
        s"""$m0, $c0, $sigma"""
      case OuParameter(m, c, a, s, t) => s"$m, $c, $a, $s, ${t.mkString(", ")}"
    }
  }

  implicit def parameterShow(implicit S: Show[SdeParameter]) = new Show[Parameters] {
    def show(p: Parameters): String = p match {
      case LeafParameter(v, sde) => v.map(x => s"$x, ").getOrElse("") + S.show(sde)
      case BranchParameter(l, r) => show(l) + ", " + show(r)
      case EmptyParameter => ""
    }
  }

  implicit def stateSpaceShow(implicit S: Show[State]) = new Show[StateSpace] {
    def show(a: StateSpace): String = s"${a.time}, ${S.show(a.state)}"
  }

  implicit def itersShow(implicit S: Show[Parameters], T: Show[StateSpace]) = new Show[MetropState] {
    def show(a: MetropState): String = 
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def paramStateShow(implicit S: Show[Parameters]) = new Show[ParamsState] {
    def show(a: ParamsState): String = 
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def filterShow(implicit S: Show[State]) = new Show[PfState] {
    def show(a: PfState): String = a.observation match {
      case Some(y) => s"${a.t}, $y, ${a.particles.map(S.show).mkString(", ")}, ${a.ess}"
      case None => s"${a.t}, NA, ${a.particles.map(S.show).mkString(", ")}, ${a.ess}"
    }
  }

  implicit def credibleIntervalsShow = new Show[CredibleInterval] {
    def show(a: CredibleInterval): String = s"${a.lower}, ${a.upper}"
  }

  implicit def filterOutShow(implicit S: Show[State], C: Show[CredibleInterval]) = new Show[PfOut] {
    def show(a: PfOut): String = a.observation match {
      case Some(x) =>
        s"${a.time}, $x, ${a.eta}, ${C.show(a.etaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
      case None =>
        s"${a.time}, NA, ${a.eta}, ${C.show(a.etaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
    }
  }

  implicit def forecastOutShow(implicit S: Show[State]) = new Show[ForecastOut] {
    def show(a: ForecastOut): String = s"${a.t}, ${a.obs}, ${a.obsIntervals.toString}, ${a.eta}, ${a.etaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
  }
}
