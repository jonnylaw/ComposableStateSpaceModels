import com.github.jonnylaw.model._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.scalactic.TolerantNumerics
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent._

/**
  * Property based tests for resampling methods
  */
object SamplingScalaCheck extends Properties("ParticleFilter") {
  // write a generator for a non-empty Collection of doubles between 0 and 1 (representing probabilities)
  val probability = Gen.choose(0.0, 1.0)
  val unnormalisedProbs = Gen.nonEmptyContainerOf[Vector, Double](probability)

  property("multinomial resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    Resampling.serialMultinomialResampling(w, w).size == w.size
  }

  property("Tree systematic resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    Resampling.treeSystematicResampling(w, w).size == w.size
  }

  property("Tree stratified resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    Resampling.treeStratifiedResampling(w, w).size == w.size
  }

  val asyncListProbs = Gen.nonEmptyContainerOf[Vector, Double](probability).
    suchThat(_.size >= 4)

  property("Tree async resampling sample should return a list of the same length") = Prop.forAll(asyncListProbs) { w =>
    Await.result(Resampling.asyncTreeSystematicResampling(4)(w, w), 10.seconds).size == w.size
  }

  // property("tree Residual resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
  //   Resampling.treeResidualResampling(w, w).size == w.size
  // }

  // val epsilon = 1e-4f
  // implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  // // take the log of the unnormalised probabilities
  // val logLikelihoods = unnormalisedProbs map (_.map(math.log))
}
