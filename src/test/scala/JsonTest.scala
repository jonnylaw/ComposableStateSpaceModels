import cats.implicits._
import breeze.linalg.DenseVector
import com.github.jonnylaw.model._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary
import spray.json._

class JsonSuite extends Properties("Json") with DataProtocols {
  val denseVector = (n: Int) => Gen.containerOfN[Array, Double](n, arbitrary[Double]).
    map(a => DenseVector(a))

  val genLeafState = for {
    v <- denseVector(1)
  } yield Tree.leaf(v)

  val genBranchState = for {
    left <- genLeafState
    right <- genLeafState
  } yield left |+| right

  val genState: Gen[State] = Gen.oneOf(genBranchState, genLeafState)

  property("toJson should serialise State to Json") = Prop.forAll(genState) { x0 =>
    x0 == x0.toJson.compactPrint.parseJson.convertTo[State]
  }

  val genBrownian: Gen[SdeParameter] = for {
    v <- arbitrary[Double]
  } yield SdeParameter.brownianParameter(v, v, v, v)

  val genOrnstein: Gen[SdeParameter] = for {
    v <- arbitrary[Double]
  } yield SdeParameter.ouParameter(v, v, v, v, v)

  val genSde: Gen[SdeParameter] = Gen.oneOf(genBrownian, genOrnstein)

  val genLeaf = for {
    v <- Gen.oneOf(arbitrary[Double] map (Some(_)), Gen.const(None))
    sde <- genSde
  } yield Parameters.leafParameter(v, sde)

  val genBranch = for {
    left <- genLeaf
    right <- genLeaf
  } yield left |+| right

  val genParams: Gen[Parameters] = Gen.oneOf(genLeaf, genBranch)

  property("toJson should serialise Parameters to Json") = Prop.forAll(genParams) { p =>
    Parameters.isIsomorphic(p, p.toJson.compactPrint.parseJson.convertTo[Parameters])
  }
}
