import com.github.jonnylaw.model._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import org.scalactic.TolerantNumerics
/**
  * Property based tests for resampling methods
  */
object SamplingScalaCheck extends Properties("ParticleFilter") {
  // write a generator for a non-empty Collection of doubles between 0 and 1 (representing probabilities)
  val probability = Gen.choose(0.0, 1.0)
  val unnormalisedProbs = Gen.nonEmptyContainerOf[Vector, Double](probability)

  property("multinomial resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    ParticleFilter.serialMultinomialResampling(w, w).size == w.size
  }

  property("systematic resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    ParticleFilter.systematicResampling(w, w).size == w.size
  }

  property("stratified resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
    ParticleFilter.stratifiedResampling(w, w).size == w.size
  }

  // property("residual resampling sample should return a list of the same length") = Prop.forAll(unnormalisedProbs) { w =>
  //   ParticleFilter.residualResampling(w, w).size == w.size
  // }

  // val epsilon = 1e-4f
  // implicit val doubleEq = TolerantNumerics.tolerantDoubleEquality(epsilon)

  // // take the log of the unnormalised probabilities
  // val logLikelihoods = unnormalisedProbs map (_.map(math.log))
}
