import breeze.stats.distributions.Rand

import cats._
import cats.implicits._
import cats.laws.discipline._

import com.github.jonnylaw.model._
import Tree._

import org.scalacheck._
import Arbitrary.arbitrary

import org.scalatest._
import org.typelevel.discipline.scalatest.Discipline
import spire.implicits._

class TreeTests extends FunSuite with Matchers with Discipline {
  def genLeaf[A](implicit a: Arbitrary[A]) =
    arbitrary[A] map (v => Tree.leaf(v))

  def genBranch[A](level: Int)(implicit a: Arbitrary[A]) =
    for {
      l <- genTree(level)(a)
      r <- genTree(level)(a)
    } yield l |+| r

  def genTree[A](level: Int)(implicit a: Arbitrary[A]): Gen[Tree[A]] = 
    if (level >= 10) genLeaf(a) else Gen.oneOf(genLeaf(a), genBranch(level + 1)(a))

  implicit def genTreeArb[A](implicit a: Arbitrary[A]): Arbitrary[Tree[A]] = Arbitrary(genTree(10)(a))

  implicit def eqTree[A](implicit t: Eq[A]): Eq[Tree[A]] = new Eq[Tree[A]] {
    def eqv(x: Tree[A], y: Tree[A]): Boolean = {
      x.flatten == x.flatten
    }
  }

  checkAll("Tree[Double]", FunctorTests[Tree].functor[Double, Double, Double])
}
