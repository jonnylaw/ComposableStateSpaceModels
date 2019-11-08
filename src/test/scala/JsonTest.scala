package parametertest

import breeze.linalg.DenseVector
import com.github.jonnylaw.model._
import DataProtocols._
import Tree._
import Parameters._
import SdeParameter._
import Sde._
import cats.Eq
import cats.implicits._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary
import spire.algebra._
import spire.implicits._
import spray.json._

class JsonSuite extends Properties("Json") with ParameterGen {
  val genLeafState = for {
    v <- denseVector(1)
  } yield Tree.leaf(v)

  val genBranchState = for {
    left <- genLeafState
    right <- genLeafState
  } yield left |+| right

  val genState: Gen[State] = Gen.oneOf(genBranchState, genLeafState)

  property("toJson should serialise State to JSON") = Prop.forAll(genState) { x0 =>
    x0 === x0.toJson.compactPrint.parseJson.convertTo[State]
  }

  property("toJson should serialise Parameters to JSON") = Prop.forAll(parameters) { p =>
    p === p.toJson.compactPrint.parseJson.convertTo[Parameters]
  }

  val genObservationWithState = for {
    t <- arbitrary[Double]
    y <- arbitrary[Double]
    eta <- arbitrary[Double]
    gamma <- arbitrary[Double]
    sdeState <- genState
  } yield ObservationWithState(t, Some(y), eta, gamma, sdeState)

  property("toJson should serialise Data to JSON") = Prop.forAll(genObservationWithState) { d =>
    d.toJson.compactPrint.parseJson.convertTo[TimedObservation] == TimedObservation(d.t, d.observation)
  }

  val genMetropState = for {
    ll <- arbitrary[Double]
    p <- parameters
    t <- arbitrary[Double]
    sde <- genState
    accepted <- arbitrary[Int]
  } yield MetropState(ll, p, StateSpace(t, sde), accepted)

  property("toJson should serialise MetropState to JSON") = Prop.forAll(genMetropState) { d =>
    val parsed = d.toJson.compactPrint.parseJson.convertTo[MetropState[Parameters, State]]

    parsed.ll === d.ll &&
    parsed.accepted === d.accepted &&
    parsed.params === d.params &&
    parsed.state === d.state.state
  }
}
