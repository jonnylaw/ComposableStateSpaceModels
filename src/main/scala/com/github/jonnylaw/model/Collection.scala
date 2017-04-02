package com.github.jonnylaw.model

import scala.language.implicitConversions
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable.TreeMap
import simulacrum._
import Collection.ops._

/**
  * A typeclass representing a Collection with a few additional 
  * features required for implementing the particle filter
  */
@typeclass trait Collection[F[_]] {
  def pure[A](a: A): F[A]
  def isEmpty[A](fa: F[A]): Boolean
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def scanLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): F[B]
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  def get[A](fa: F[A])(i: Int): A
  def indices[A](fa: F[A]): F[Int]
  def indexWhere[A](fa: F[A])(cond: A => Boolean): Int
  def append[A](fa: F[A])(a: A): F[A]
  def max[A: Ordering](fa: F[A]): A
  def size[A](fa: F[A]): Int
  def fill[A](n: Int)(a: => A): F[A]
  def toArray[A: ClassTag](fa: F[A]): Array[A]
  def toVector[A](fa: F[A]): Vector[A]
  def unzip[A, B](fa: F[(A, B)]): (F[A], F[B])
  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  def drop[A](fa: F[A])(n: Int): F[A]
  def toTreeMap[A: Ordering, B](fa: F[(A, B)]): TreeMap[A, B]
}
