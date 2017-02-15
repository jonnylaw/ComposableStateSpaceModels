package com.github.jonnylaw

import breeze.stats.distributions.{Rand, Process}
import breeze.stats.distributions.Rand._
import breeze.linalg.{DenseVector, diag}
import cats._
import cats.implicits._
import cats.data.Reader
import fs2.Stream
import scala.collection.parallel.immutable.ParVector
import scala.reflect.ClassTag

package object model {
  type Observation = Double
  type Eta = Double
  type Gamma = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type UnparamModel = Reader[Parameters, Model]
  type UnparamSde = Reader[SdeParameter, Sde]
  type StepFunction = (SdeParameter) => (State, TimeIncrement) => Rand[State]
  type State = Tree[DenseVector[Double]]
  type Resample[F[_], A] = (F[A], F[LogLikelihood]) => F[A]

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

  /**
    * A typeclass representing a Collection with a few additional 
    * features required for implementing the particle filter
    */
  trait Collection[F[_]] extends Foldable[F] with MonoidK[F] with Monad[F] {
    def scanLeft[A, B, C](fa: F[A], z: B)(f: (B, A) => B): F[B]
    def get[A](fa: F[A])(i: Int): A
    def indices[A](fa: F[A]): F[Int]

    /**
      * Expand a into a range of Numerics
      */
    def range[A](from: A, to: A, by: A)(implicit N: Numeric[A], f: MonoidK[F], app: Applicative[F]) = {
      def loop(acc: F[A], current: A): F[A] = {
        if (N.gteq(current, to)) {
          acc
        } else {
          loop(f.combineK(acc, app.pure(N.plus(current, by))), N.plus(current, by))
        }
      }
      loop(app.pure(from), from)
    }

    def indexWhere[A](fa: F[A])(cond: A => Boolean): Int

    def max[A: Ordering](fa: F[A]): A

    /**
      * Fund the sum of numeric values in a foldable
      */
    def sum[A](l: F[A])(implicit F: Foldable[F], N: Numeric[A]): A = {
      F.foldLeft(l, N.zero)((a, b) => N.plus(a, b))
    }

    /**
      * Generic Mean Function
      */
    def mean[A](s: F[A])(implicit f: Foldable[F], N: Fractional[A]): A = {
      N.div(sum(s), N.fromInt(s.size.toInt))
    }

    def fill[A](n: Int)(a: A): F[A]
    
    def toArray[A: ClassTag](fa: F[A]): Array[A]

    def toList[A](fa: F[A]): List[A]

    def unzip[A, B](fa: F[(A, B)]): (F[A], F[B])
  }


  implicit def parVectorCollection = new Collection[ParVector] {
    def pure[A](x: A): ParVector[A] = ParVector(x)

    def flatMap[A, B](fa: ParVector[A])(f: A => ParVector[B]): ParVector[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => ParVector[Either[A,B]]): ParVector[B] = ???

    // Members declared in Collection
    def get[A](fa: ParVector[A])(i: Int): A = fa(i)
    def indexWhere[A](fa: ParVector[A])(cond: A => Boolean): Int = fa.indexWhere(cond)
    def scanLeft[A, B, C](fa: ParVector[A],z: B)(f: (B, A) => B): ParVector[B] = fa.scanLeft(z)(f)

    // Members declared in cats.Foldable
    def foldLeft[A, B](fa: ParVector[A],b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    def foldRight[A, B](fa: ParVector[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = fa.foldRight(lb)(f)

    // Members declared in cats.MonoidK
    def empty[A]: ParVector[A] = ParVector()

    // Members declared in cats.SemigroupK
    def combineK[A](x: ParVector[A], y: ParVector[A]): ParVector[A] = x ++ y

    def indices[A](fa: ParVector[A]) = fa.zipWithIndex.map(_._2)

    def toArray[A: ClassTag](fa: ParVector[A]): Array[A] = fa.toArray

    def fill[A](n: Int)(a: A): ParVector[A] = ParVector.fill(n)(a)
    def unzip[A, B](fa: ParVector[(A, B)]): (ParVector[A], ParVector[B]) = fa.unzip
    def max[A: Ordering](fa: ParVector[A]): A = fa.max
  }

  implicit def listCollection = new Collection[List] {
    def pure[A](x: A): List[A] = List(x)

    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => List[Either[A,B]]): List[B] = ???

    // Members declared in Collection
    def get[A](fa: List[A])(i: Int): A = fa(i)
    def indexWhere[A](fa: List[A])(cond: A => Boolean): Int = fa.indexWhere(cond)
    def scanLeft[A, B, C](fa: List[A],z: B)(f: (B, A) => B): List[B] = fa.scanLeft(z)(f)

    // Members declared in cats.Foldable
    def foldLeft[A, B](fa: List[A],b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
    def foldRight[A, B](fa: List[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = fa.foldRight(lb)(f)

    // Members declared in cats.MonoidK
    def empty[A]: List[A] = List()

    // Members declared in cats.SemigroupK
    def combineK[A](x: List[A], y: List[A]): List[A] = x ++ y

    /**
      * Return the indices of a collection
      */
    def indices[A](fa: List[A]) = fa.zipWithIndex.map(_._2)

    def fill[A](n: Int)(a: A) = List.fill(n)(a)
    def toArray[A: ClassTag](fa: List[A]): Array[A] = fa.toArray
    def unzip[A, B](fa: List[(A, B)]): (List[A], List[B]) = fa.unzip
    def max[A: Ordering](fa: List[A]): A = fa.max
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
      case TimedObservation(t, y) => s"$t, $y"
      case ObservationWithState(t, y, e, g, x) => s"$t, $y, $e, $g, ${S.show(x)}"
    }
  }

  implicit def sdeParamShow = new Show[SdeParameter] {
    def show(p: SdeParameter): String = p match {
      case BrownianParameter(m0, c0, mu, sigma) =>
        s"""${m0.data.mkString(", ")}, ${c0.data.mkString(", ")}, ${mu.data.mkString(", ")}, ${diag(sigma).data.mkString(", ")}"""
      case OrnsteinParameter(m0, c0, theta, alpha, sigma) =>
        s"""${m0.data.mkString(", ")}, ${c0.data.mkString(", ")}, ${theta.data.mkString(", ")}, ${alpha.data.mkString(", ")}, ${sigma.data.mkString(", ")}"""
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

  implicit def filterShow[F[_]: Collection](implicit S: Show[State]) = new Show[PfState[F]] {
    def show(a: PfState[F]): String = a.observation match {
      case Some(y) => s"${a.t}, $y, ${(a.particles map (S.show)).toList.mkString(", ")}, ${a.ess}"
      case None => s"${a.t}, NA, ${(a.particles map (S.show)).toList.mkString(", ")}, ${a.ess}"
    }
  }

  implicit def credibleIntervalsShow = new Show[CredibleInterval] {
    def show(a: CredibleInterval): String = s"${a.lower}, ${a.upper}"
  }

  implicit def filterOutShow(implicit S: Show[State], C: Show[CredibleInterval]) = new Show[PfOut] {
    def show(a: PfOut): String = a.observation match {
      case Some(x) =>
        s"${a.time}, $x, ${a.gamma}, ${C.show(a.gammaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
      case None =>
        s"${a.time}, NA, ${a.gamma}, ${C.show(a.gammaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
    }
  }

  implicit def forecastOutShow(implicit S: Show[State]) = new Show[ForecastOut] {
    def show(a: ForecastOut): String = s"${a.t}, ${a.obs}, ${a.obsIntervals.toString}, ${a.gamma}, ${a.gammaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
  }

  implicit def fromProcess[F[_], A](iter: Process[A]): Stream[F, A] = {
    Stream.unfold(iter.step){ case (a, p) => Some((a, p.step)) }
  }
}
