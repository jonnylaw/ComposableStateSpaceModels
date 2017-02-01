// import org.scalatest._

// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.stream.scaladsl.Source
// import akka.stream.scaladsl._
// import scala.concurrent.{Future, Await}
// import scala.concurrent.duration._
// import java.nio.file.{Paths, Path}

// class AkkaStreamTest extends FlatSpec with Matchers {
//   import scala.concurrent.ExecutionContext.Implicits.global
//   implicit val system = ActorSystem("TestActors")
//   implicit val materializer = ActorMaterializer()

//   "Burnin" should "drop some items from the start of a stream" in {
//     val stream = Source(Stream.from(1))
//     val future = stream.via(burnin(10)).take(1).runWith(Sink.head)
//     val result = Await.result(future, 100.millis)

//     assert(result == 11)
//   }

//   "thin" should "remove every other element from a stream" in {
//     val stream = Source(Stream.from(1))
//     val future = stream.via(thinParameters(5)).grouped(10).runWith(Sink.head)
//     val result = Await.result(future, 100.millis)

//     assert(result == (5 to 50 by 5).toVector)
//   }

//   "average" should "calculate the average of a stream of lists" in {
//     // make a vector of vectors Vector(Vector(1,1,1,1,1), ..., Vector(5,5,5,5,5
//     val init = (1 to 5) map (i => Array.fill(5)(i.toDouble))
//     val stream = Source(init)
//     val future = stream.via(calculateAverageOfParameters(5)).take(1).runWith(Sink.head)
//     val result = Await.result(future, 100.millis)

//     assert(result == Vector.fill(5)(3.0))
//   }

//   // "read parameter stream" should "read parameters from a file as a stream" in {
//   //   val params = (1 to 100) map (i => Vector.fill(5)(i.toDouble))

//   //   val f = new File("parameters.csv")
//   //   val pw = new PrintWriter(f)
//   //   pw.write(params.map(_.mkString(",")).mkString("\n"))
//   //   pw.close()

//   //   val future = readParameterStream(f).take(1).runWith(Sink.head)
//   //   val result = Await.result(future, 100.millis)

//   //   f.delete()

//   //   assert(result.head == 1.0)
//   //   assert(result.map(_.toDouble) == Array.fill(5)(1.0))
//   // }

//   "Average parameters stream" should "read parameters from a file as a stream and calculate the average" in {
//     val params = (1 to 100) map (i => Vector.fill(5)(i.toDouble))

//     val f = new File("parameters.csv")
//     val pw = new PrintWriter(f)
//     pw.write(params.map(_.mkString(",")).mkString("\n"))
//     pw.close()

//     val future = cleanParameterFlow(Paths.get("parameters.csv"), 10, 1, 100).run
//     val result = Await.result(future, 1000.millis)

//     f.delete()

//     val average = breeze.stats.mean((11 to 100).map(_.toDouble).toVector)

//     assert(result == Vector.fill(5)(average))
//   }
// }
