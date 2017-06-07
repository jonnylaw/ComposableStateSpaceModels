// package parametertest

// import cats.implicits._
// import breeze.linalg.DenseVector
// import com.github.jonnylaw.model._
// import org.scalacheck.Prop.forAll
// import org.scalacheck._
// import Arbitrary.arbitrary
// import spray.json._
// import DataProtocols._

// class OpticsSuite extends Properties("Json") with ParameterGen {

//   val input = for {
//     p <- genParameters
//     newSigma <- denseVector(1)
//   } yield (p, newSigma)

//   property("getSet property for sigma") = forAll(input) { case (p, s) =>
    
//   }
// }
