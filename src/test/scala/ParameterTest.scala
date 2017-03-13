import breeze.linalg.{DenseVector, DenseMatrix}
import cats.implicits._
import com.github.jonnylaw.model._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary

/**
  * Property based tests for resampling methods
  */
object ParameterFunctionSuite extends Properties("Parameters") {
  val denseVector = (n: Int) => Gen.containerOfN[Array, Double](n, arbitrary[Double]).
    map(a => DenseVector(a))

  val genBrownian: Int => Gen[SdeParameter] = (n: Int) => for {
    v <- denseVector(n)
  } yield SdeParameter.brownianParameter(v, v, v, v)

  val genOrnstein: Int => Gen[SdeParameter] = (n: Int) => for {
    v <- denseVector(n)
  } yield SdeParameter.ornsteinParameter(v, v, v, v, v)

  val genSde: Int => Gen[SdeParameter] = (n: Int) => Gen.oneOf(genBrownian(n), genOrnstein(n))

  val genLeaf = (n: Int) => for {
    v <- arbitrary[Double]
    sde <- genSde(n)
  } yield Parameters.leafParameter(Some(v), sde)

  val genBranch = (n: Int) => for {
    left <- genLeaf(n)
    right <- genLeaf(n)
  } yield left |+| right

  val input1 = (n: Int) => for {
    sde <- genSde(n)
    d <- denseVector(sde.length)
  } yield (sde, d)

  property("Add to sde Parameter") = Prop.forAll(input1(1)) { case (sde, d) =>
    sde.add(d) == sde.add(d)
  }

  val input = (n: Int) => for {
    p <- genBranch(n) 
    d <- denseVector(p.length)
  } yield (p, d)

  property("add should add values to parameters in a tree") = Prop.forAll(input(1)) { case (p, d) =>
    p.add(d) == p.add(d)
  }
}
