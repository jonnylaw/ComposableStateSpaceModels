import cats.implicits._
import breeze.linalg.DenseVector
import com.github.jonnylaw.model._
import org.scalatest._
import spray.json._

// TODO: Convert this into scalacheck
class JsonSuite extends FlatSpec with Matchers with DataProtocols {
  "Nested State" should "Serialise and deserialise to JSON" in {
    val x0: State = Tree.leaf(DenseVector(1.0, 2.0, 3.0)) |+| Tree.leaf(DenseVector(2.0))

    assert(x0 == x0.toJson.compactPrint.parseJson.convertTo[State])
  }

  "Nested Parameters" should "Serialise and deserialise to JSON" in {
    val sdeParameter = SdeParameter.brownianParameter(
      DenseVector(1.0),
      DenseVector(1.0),
      DenseVector(1.0),
      DenseVector(1.0))
    val p: Parameters = Parameters.leafParameter(None, sdeParameter)

    val combParams = p |+| p

    assert(p == p.toJson.compactPrint.parseJson.convertTo[Parameters])
  }
}
