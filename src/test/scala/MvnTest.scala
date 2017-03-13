import breeze.linalg._
import breeze.stats._
import breeze.stats.distributions._
import com.github.jonnylaw.model._
import org.scalacheck.Prop.forAll
import org.scalacheck._
import Arbitrary.arbitrary


object MvnTest extends Properties("MultivariateNormal") {
  val denseVector = (n: Int) => Gen.containerOfN[Array, Double](n, arbitrary[Double]).
    map(a => DenseVector(a))
  val denseMatrix = (n: Int) => Gen.containerOfN[Array, Double](n * n, arbitrary[Double]).
    map(a => new DenseMatrix(n, n, a))
  val symmetricPosDefMatrix = (n: Int) => denseVector(n).
    flatMap(z => denseVector(n).map(a => a * a.t).suchThat(m => z.t * m * z > 0))

  val input = for {
    mean <- denseVector(2)
    cov <- symmetricPosDefMatrix(2)
  } yield (mean, cov)

  val covariance = Gen.const(DenseMatrix((0.666, -0.333), (-0.333, 0.666)))
  
  // mean zero and covariance the given matrix, for some reason the off diagonals are much smaller than expected
  property("draw should sample realisations from the MVN distribution") = Prop.forAll(covariance) { cov =>
    val n = 10000
    val samples = MultivariateNormal(DenseVector.zeros[Double](2), cov).sample(n)

    val res = Utilities.meanCovSamples(samples)

    norm(res._1) < 0.1
    sum(res._2 - cov) < 0.1
  }
}
