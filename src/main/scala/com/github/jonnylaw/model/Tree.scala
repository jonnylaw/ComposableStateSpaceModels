package com.github.jonnylaw.model

import breeze.linalg.DenseVector
import cats.{Functor, Applicative, Apply}
import scala.language.higherKinds
import spire.implicits._
import spire.algebra._

/**
  * A binary tree implementation, to be used when combining models
  * Hopefully this simplifies "zooming" into values and changing them
  */
sealed trait Tree[+A] { self =>
  import Tree._

  /**
    * Concatenate two trees
    */
  def +++[B >: A](that: Tree[B]): Tree[B] = {
    branch(self, that)
  }

  /**
    * Get the contents of the leaf node element at position n from the left, indexed from 0
    * @param n the node position from the left
    */
  def getNode(n: Int): A = {
    val l = self.flatten
    l(n)
  }

  /**
    * Reduce the tree to a value, by recursively applying fold to the branches, combining the results using g
    * and transforming the leaves using f
    */
  def fold[B](z: B)(f: A => B)(g: (B, B) => B): B = {
    def loop(acc: B, remaining: Tree[A]): B = remaining match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(loop(acc, l), loop(acc, r))
      case Empty        => acc
    }

    loop(z, self)
  }

  def foldMap[B >: A](f: B => B)(implicit M: Monoid[B]): B = {
    self.fold(M.id)(identity)((b, a) => M.op(b, f(a)))
  }

  def flatten: List[A] = self match {
    case Leaf(a)      => List[A](a)
    case Branch(l, r) => l.flatten ++ r.flatten
    case Empty        => List.empty
  }

  /**
    * Combine two trees which are the same shape using the function f
    */
  def zipWith[B, C](that: Tree[B])(f: (A, B) => C): Tree[C] = (self, that) match {
    case (Leaf(a), Leaf(b))             => leaf(f(a, b))
    case (Branch(l, r), Branch(l1, r1)) => branch(l.zipWith(l1)(f), r.zipWith(r1)(f))
    case _                              => throw new Exception("Can't zip different shaped trees")
  }

  def prettyPrint: String = self match{
    case Branch(l, r) => "/ \\ \n" ++ l.prettyPrint ++ r.prettyPrint
    case Leaf(a)      => a.toString
    case Empty        => ""
  }

  /**
    * Add two (same shape) trees together, each leaf node is added
    */
  def plus[B >: A](that: Tree[B])(implicit S: AdditiveSemigroup[B]): Tree[B] = 
    self.zipWith(that)(S.plus)
}
case class  Leaf[A](value: A)                        extends Tree[A]
case class  Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
case object Empty                                    extends Tree[Nothing]

object Tree {
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)

  def leaf[A](a: A): Tree[A] = Leaf(a)

  def empty: Tree[Nothing] = Empty

  /**
    * Traverse a binary tree with an applicative function f: A => F[B] and return an F[Tree[B]]
    * This is similar to map, but a map would return a Tree[F[B]]
    * @param ta a Tree to be traversed
    * @param f a function from the type at the tree leaves to an applicative of any type
    * @return a structure with the form F[Tree[B]] 
    */
  def traverse[F[_]: Applicative, A, B](ta: Tree[A])(f: A => F[B]): F[Tree[B]] = ta match {
    case Leaf(v)      => Applicative[F].map(f(v))(Tree.leaf)
    case Branch(l, r) => Applicative[F].map2(traverse(l)(f), traverse(r)(f)){ Tree.branch(_, _) }
    case Empty        => Applicative[F].pure(empty)
  }

  implicit def treeFunctor = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v)      => Tree.leaf(f(v))
      case Branch(l, r) => Tree.branch(map(l)(f), map(r)(f))
      case Empty        => Empty
    }
  }

  /**
    * fill the tree from the left
    */
  def constructTreeLeft[A](l: Seq[A]): Tree[A] = {
    l.tail.foldLeft(Tree.leaf(l.head))(_ +++ Tree.leaf(_))
  }

  def isIsomorphic[A](t: Tree[A], t1: Tree[A]): Boolean = {
    t.flatten == t1.flatten
  }

  /**
    * Monoid to build a larger tree
    */
  implicit def composeTreeMonoid[A] = new Monoid[Tree[A]] {
    def op(l: Tree[A], r: Tree[A]): Tree[A] = branch(l, r)
    def id: Tree[Nothing] = empty
  }

  implicit def treeAddSemigroup[A: AdditiveSemigroup] = new AdditiveSemigroup[Tree[A]] {
    def plus(x: Tree[A], y: Tree[A]) = x plus y
  }

  implicit def eqTree[A](implicit t: Eq[A]): Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = {
      x.flatten == x.flatten
    }
  }
}
