package com.github.jonnylaw.model

import cats._

sealed trait Tree[+A]
case class Leaf[+A](a: A) extends Tree[A]
case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)

  implicit def treeSemigroup[A] = new Semigroup[Tree[A]] {
    def combine(t: Tree[A], s: Tree[A]): Tree[A] = {
      branch(t, s)
    }
  }

  implicit def treeMonad = new Monad[Tree] with Traverse[Tree] {
    def pure[A](a: A): Tree[A] = Leaf(a)

    def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(a) => f(a)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    // TODO: Implement tailRecM properly...
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A,B]]): Tree[B] = f(a) match {
      case Leaf(Right(b)) => Leaf(b)
      case Leaf(Left(a1)) => tailRecM(a1)(f)
      case _ => throw new Exception("I have no idea what will happen here")
    }

    def foldLeft[A, B](fa: Tree[A],b: B)(f: (B, A) => B): B = ???
    def foldRight[A, B](fa: Tree[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???

    def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit evidence: cats.Applicative[G]): G[Tree[B]] = ???
  }


}
