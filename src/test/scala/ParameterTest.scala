package parametertest

import breeze.linalg.{DenseVector, DenseMatrix}
import cats.implicits._
import com.github.jonnylaw.model._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary

trait ParameterGen {
  val denseVector = (n: Int) => Gen.containerOfN[Array, Double](n, arbitrary[Double]).
    map(a => DenseVector(a))

  val genBrownian: Gen[SdeParameter] = for {
    v <- Gen.nonEmptyContainerOf[List, Double](arbitrary[Double])
  } yield SdeParameter.brownianParameter(v: _*)(v: _*)(v: _*)

  val genOrnstein: Gen[SdeParameter] = for {
    v <- Gen.nonEmptyContainerOf[List, Double](arbitrary[Double])
  } yield SdeParameter.ouParameter(v: _*)(v: _*)(v: _*)(v: _*)(v: _*)

  val genGenBrownian: Gen[SdeParameter] = for {
    v <- Gen.nonEmptyContainerOf[List, Double](arbitrary[Double])
  } yield SdeParameter.genBrownianParameter(v: _*)(v: _*)(v: _*)(v: _*)

  val genSde: Gen[SdeParameter] = Gen.oneOf(genGenBrownian, Gen.oneOf(genBrownian, genOrnstein))

  val genLeaf = for {
    v <- arbitrary[Double]
    sde <- genSde
  } yield Parameters.leafParameter(Some(v), sde)

  def genBranch(level: Int) = for {
    left <- genParameters(level)
    right <- genParameters(level)
  } yield left |+| right

  def genParameters(level: Int): Gen[Parameters] = if (level >= 10) genLeaf else Gen.oneOf(genLeaf, genBranch(level + 1))

  lazy val parameters: Gen[Parameters] = genParameters(0)
}

object ParameterFunctionSuite extends Properties("Parameters") with ParameterGen {
  val input = (n: Int) => for {
    p <- parameters
    d <- denseVector(p.length)
    e <- denseVector(p.length)
  } yield (p, d, e)

  property("add on parameters should be commutative") = forAll(input(1)) { case (p, d, e) =>
    p.add(d).sum(p.add(e)) == p.add(e).sum(p.add(d))
  }

  val input1 = (n: Int) => for {
    sde <- genSde
    d <- denseVector(sde.length)
    e <- denseVector(sde.length)
  } yield (sde, d, e)

  property("add on sde parameters should be commutative") = forAll(input1(1)) { case (sde, d, e) =>
    sde.add(d).sum(sde.add(e)) == sde.add(e).sum(sde.add(d))
  }
}
