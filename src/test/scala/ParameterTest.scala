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
    v <- arbitrary[Double]
  } yield SdeParameter.brownianParameter(v, v, v)

  val genOrnstein: Gen[SdeParameter] = for {
    v <- arbitrary[Double]
    theta <- Gen.containerOf[List, Double](arbitrary[Double])
  } yield SdeParameter.ouParameter(v, v, v, v)(theta: _*)

  val genGenBrownian: Gen[SdeParameter] = for {
    v <- arbitrary[Double]
  } yield SdeParameter.genBrownianParameter(v, v, v, v)

  val genSde: Gen[SdeParameter] = Gen.oneOf(genGenBrownian, Gen.oneOf(genBrownian, genOrnstein))

  val genLeaf = for {
    v <- arbitrary[Double]
    sde <- genSde
  } yield Parameters.leafParameter(Some(v), sde)

  val genBranch = for {
    left <- genLeaf
    right <- genLeaf
  } yield left |+| right
}

object ParameterFunctionSuite extends Properties("Parameters") with ParameterGen {
  val input = (n: Int) => for {
    p <- genBranch 
    d <- denseVector(p.length)
  } yield (p, d)

  property("add should add values to parameters in a tree") = forAll(input(1)) { case (p, d) =>
    p.add(d) == p.add(d)
  }

  val input1 = (n: Int) => for {
    sde <- genSde
    d <- denseVector(sde.length)
  } yield (sde, d)

  property("Add to sde Parameter") = forAll(input1(1)) { case (sde, d) =>
    sde.add(d) == sde.add(d)
  }
}
