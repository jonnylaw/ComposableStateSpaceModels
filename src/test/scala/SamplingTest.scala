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
    Resampling.multinomialResampling(w, w).size == w.size
  }

  property("systematic resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    Resampling.systematicResampling(w, w).size == w.size
  }

  property("stratified resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    Resampling.stratifiedResampling(w, w).size == w.size
  }
}
