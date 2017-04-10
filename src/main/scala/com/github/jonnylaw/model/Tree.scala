package com.github.jonnylaw.model

import cats._

/**
  * A binary tree implementation, to be used when combining models
  * Hopefully this simplifies "zooming" into values and changing them
  */
sealed trait Tree[A] { self =>
  import Tree._

  /**
    * Add a Tree to the right of the tree
    */
  def |+(that: Tree[A]): Tree[A] = {
    branch(self, that)
  }

  /**
    * Add a Tree to the left of the tree
    */
  def +|(that: Tree[A]): Tree[A] = {
    branch(that, self)
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
      case Leaf(a) => f(a)
      case Branch(l, r) => g(loop(acc, l), loop(acc, r))
    }

    loop(z, self)
  }

  def foldMap(f: A => A)(implicit m: Monoid[A]): A = {
    self.fold(m.empty)(identity)((b, a) => m.combine(b, f(a)))
  }

  def flatten: List[A] = self match {
    case Leaf(a) => List[A](a)
    case Branch(l, r) => l.flatten ++ r.flatten
  }

  /**
    * Combine two trees which are the same shape using the function f
    */
  def zipWith[B, C](that: Tree[B])(f: (A, B) => C): Tree[C] = (self, that) match {
    case (Leaf(a), Leaf(b)) => leaf(f(a, b))
    case (Branch(l, r), Branch(l1, r1)) => branch(l.zipWith(l1)(f), r.zipWith(r1)(f))
    case _ => throw new Exception("Can't zip different shaped trees")
  }

  def prettyPrint: String = self match{
    case Branch(l, r) => "/ \\ \n" ++ l.prettyPrint ++ r.prettyPrint
    case Leaf(a) => a.toString
  }

  /**
    * Add two (same shape) trees together, each leaf node is added
    */
  def add(that: Tree[A])(implicit N: Numeric[A]): Tree[A] = self.zipWith(that)(N.plus)
}
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  def leaf[A](a: A): Tree[A] = Leaf(a)

  /**
    * fill the tree from the left
    * 
    */
  def constructTreeLeft[A](l: Seq[A]): Tree[A] = {
    l.tail.foldLeft(Tree.leaf(l.head))(_ |+ Tree.leaf(_))
  }

  def isIsomorphic[A: Ordering](t: Tree[A], t1: Tree[A]): Boolean = {
    t.flatten == t1.flatten
  }

  /**
    * Fill an already constructed tree
    */
//  def fillTree(t: Tree[A])(l: Seq[B]): Tree[A] = 

  implicit val treeFun = new Functor[Tree] {
    def pure[A](a: A): Tree[A] = leaf(a)

    def map[A, B](a: Tree[A])(f: A => B): Tree[B] = a match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => branch(map(l)(f), map(r)(f))
    }
  }

  /**
    * Semigroup to build a larger tree
    */
  implicit def composeTreeSemigroup[A] = new Semigroup[Tree[A]] {
    def combine(l: Tree[A], r: Tree[A]): Tree[A] = branch(l, r)
  }
}
