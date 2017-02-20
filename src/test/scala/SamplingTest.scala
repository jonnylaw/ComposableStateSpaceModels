import com.github.jonnylaw.model._
import org.scalatest._

class SamplingSuite extends FlatSpec with Matchers {
  "Systematic Resampling" should "Return a sequence of equivalent size" in {
    val w = Vector.fill(10)(1.0)

    assert(ParticleFilter.systematicResampling(w, w).size == w.size)
  }

  "Stratified Resampling" should "Return a sequence of equivalent size" in {
    val w = Vector.fill(10)(1.0)

    assert(ParticleFilter.stratifiedResampling(w, w).size == w.size)
  }

  "Multinomial Resampling" should "Return a sequence of equivalent size" in {
    val w = Vector.fill(10)(1.0)

    assert(ParticleFilter.multinomialResampling(w, w).size == w.size)
  }
}
