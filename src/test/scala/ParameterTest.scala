package parametertest

import breeze.linalg.{DenseVector, DenseMatrix}
import cats._
import cats.implicits._
import cats.kernel.laws.GroupLaws

import com.github.jonnylaw.model._
import Tree._
import Parameters._
import Sde._
import SdeParameter._
import org.scalatest._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.typelevel.discipline.scalatest.Discipline
import Arbitrary.arbitrary
import spire.algebra._
import spire.implicits._

trait ParameterGen {
  val denseVector = (n: Int) => Gen.containerOfN[Array, Double](n, arbitrary[Double]).
    map(a => DenseVector(a))

  val genBrownian: Gen[SdeParameter] = for {
    v <- Gen.containerOfN[List, Double](3, arbitrary[Double])
  } yield SdeParameter.brownianParameterUnconstrained(v: _*)(v: _*)(v: _*)

  val genOrnstein: Gen[SdeParameter] = for {
    v <- Gen.containerOfN[List, Double](3, arbitrary[Double])
  } yield SdeParameter.ouParameterUnconstrained(v: _*)(v: _*)(v: _*)(v: _*)(v: _*)

  val genGenBrownian: Gen[SdeParameter] = for {
    v <- Gen.containerOfN[List, Double](3, arbitrary[Double])
  } yield SdeParameter.genBrownianParameterUnconstrained(v: _*)(v: _*)(v: _*)(v: _*)

  val genSde: Gen[SdeParameter] = Gen.oneOf(genGenBrownian, genBrownian, genOrnstein)

  val genLeaf = for {
    v <- arbitrary[Double]
    sde <- genSde
  } yield Parameters(Some(v), sde)

  def genBranch(level: Int) = for {
    left <- genParameters(level)
    right <- genParameters(level)
  } yield left |+| right

  def genParameters(level: Int): Gen[Parameters] =
    if (level >= 10) genLeaf else Gen.oneOf(genLeaf, genBranch(level + 1))

  lazy val parameters: Gen[Parameters] = genParameters(0)
}

object ParameterFunctionSuite extends Properties("Parameters")
                              with ParameterGen {
  val input = for {
    p <- parameters
    d <- denseVector(Parameters.paramSize(p))
    e <- denseVector(Parameters.paramSize(p))
  } yield (p, d, e)

  property("add a dense vector to parameters should be commutative") = forAll(input) { case (p, d, e) =>
    val S: Addable[Parameters] = implicitly[Addable[Parameters]]
    S.add(p, d) + S.add(p, e) === S.add(p, e) + S.add(p, d)
  }

  val input1 = for {
    sde <- genSde
    d <- denseVector(sde.length)
    e <- denseVector(sde.length)
  } yield (sde, d, e)

  property("add on sde parameters should be commutative") = forAll(input1) { case (sde, d, e) =>
    val S: Addable[SdeParameter] = implicitly[Addable[SdeParameter]]
    S.add(sde, d) + S.add(sde, e) === S.add(sde, e) + S.add(sde, d)
  }
}

/**
  * Test that parameter trees can be added up
  * Commonly used when calculating summary
  */
class ParameterAdditive extends FunSuite
                        with Matchers
                        with Discipline
                        with ParameterGen {

  implicit val arbParam = Arbitrary(parameters)

  checkAll("Additive Parameter Semigroup", GroupLaws[Parameters].semigroup)
}
