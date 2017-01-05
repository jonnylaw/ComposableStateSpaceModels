package com.github.jonnylaw

import breeze.stats.distributions.Rand
import breeze.stats.distributions.Rand._
import cats.{Monad, Applicative, Traverse, Eval}

package object model {
  type Eta = Seq[Double]
  type Zeta = Double
  type Observation = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type StepFunction = SdeParameter => (State, TimeIncrement) => Rand[State]

  type Attempt[A] = Either[Throwable, A]

  implicit def bool2obs(b: Boolean): Observation = if (b) 1.0 else 0.0
  implicit def obs2bool(o: Observation): Boolean = if (o == 0.0) false else true

  implicit def randMonad = new Monad[Rand] {
    def pure[A](x: A): Rand[A] = always(x)
    def flatMap[A, B](fa: Rand[A])(f: A => Rand[B]): Rand[B] = fa.flatMap(f)
    def tailRecM[A, B](a: A)(f: A => Rand[Either[A,B]]): Rand[B] = f(a).draw match {
      case Right(b) => always(b)
      case Left(b) => tailRecM(b)(f)
    }
  }

  // implicit def randTraverse = new Traverse[Rand] {
  //   def traverse[G[_], A, B](fa: Rand[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Rand[B]] = ???

  //   def foldLeft[A, B](fa: Rand[A], b: B)(f: (B, A) => B): B = ???
  //   def foldRight[A, B](fa: Rand[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???

  // }
}
