// import org.scalatest._
// import com.github.jonnylaw.model.Utilities._

// class UtiltiesTest extends FlatSpec with Matchers {

//   "Rectangle method" should "approximate integrals in low dimensions" in {
//     val f: Double => Double = x => x*x + x + 6
//     val start = 0.0
//     val end = 5.0

//     val result = rectangleMethod(start, end, f, 3)

//     assert(math.abs(result - 505.0/6) < 0.1)
//   }

//   "Rectangle method stream" should "approximate low dimensional integrals" in {
//      val f: Double => Double = x => x*x + x + 6
//     val start = 0.0
//     val end = 5.0

//     val result = rectangleMethodStream(start, end, f, 3)

//     assert(math.abs(result - 505.0/6) < 0.1)
//   }


// }
