// package com.github.jonnylaw.examples

// import akka.stream.scaladsl._
// import akka.stream._
// import akka.NotUsed
// import akka.actor.ActorSystem
// import akka.util.ByteString
// import akka.NotUsed
// import akka.actor.ActorSystem
// import akka.http.scaladsl.Http

// import akka.http.scaladsl.model._
// import HttpEntity.ChunkStreamPart
// import Uri.Query
// import HttpMethods._
// import akka.stream.ActorMaterializer

// import akka.http.scaladsl.server.Directives._
// import akka.util.ByteString
// import GraphDSL.Implicits._
// import breeze.stats.distributions.Rand
// import cats._
// import cats.implicits._
// import com.github.jonnylaw.model._
// import java.nio.file.Files;
// import java.nio.file.Paths;
// import scala.io.StdIn
// import scala.concurrent.Future
// import scala.concurrent.duration._
// import scala.concurrent.ExecutionContext.Implicits.global
// import spray.json._
// import scala.util.{Try, Success, Failure}

// /**
//   * Sample from the joint posterior of the state and parameters p(x, theta | y)
//   * Serialize this to JSON using Spray JSON
//   * Write as invalid JSON, by converting each element of the sequence to JSON and writing them on a new line of the output file
//   * This is the same as Twitters streaming API
//   */
// object DeterminePoissonPosterior extends App with PoissonTestModel with DataProtocols {
//   val res = for {
//     data <- DataFromFile("data/PoissonModelSims.csv").
//       observations.
//       take(400).
//       runWith(Sink.seq)
//     pf = (p: Parameters) => ParticleFilter.filterLlState(
//       data.toVector,
//       Resampling.systematicResampling,
//       150)(mod(p))
//     pmmh = ParticleMetropolisHastings(pf, simPrior.draw,
//       Parameters.perturb(0.05), (a, b) => 0.0, prior)
//     iters <- pmmh.
//       iters.
//       take(10000).
//       map(_.toJson.compactPrint).
//       runWith(Streaming.writeStreamToFile("data/PoissonPosterior.json"))
//     } yield iters

//   res.
//     onComplete(_ => system.terminate())
// }

// /**
//   * Serve the data as a stream of JSON
//   */
// object PoissonServer extends App with PoissonTestModel with DataProtocols {
//   // simulate the model using a breeze process which can be viewed as an iterator
//   val sims = SimulateData(mod(poissonParam)).observations

//   // transform the iterator to an akka stream
//   val simStream = sims.
//     map(d => TimedObservation(d.t, d.observation)).
//     zip(Source.tick(1.second, 1.seconds, ())). // slow the stream down
//     map { case (d, _) => d}.
//     take(1000). 
//     map(_.toJson.prettyPrint). // convert each reading to JSON
//     map(s => ByteString(s + ",\n")). 
//     map(s => ChunkStreamPart(s))

//   // serve the application on port 8080
//   val serverSource = Http().bind("localhost", 8080)

//   val requestHandler: HttpRequest => HttpResponse = {
//     case HttpRequest(GET, Uri.Path("/"), _, _, _) =>
//       HttpResponse(entity = 
//         HttpEntity.Chunked(ContentTypes.`text/html(UTF-8)`, simStream)
//       )
//   }

//   val bindingFuture = serverSource.to(Sink.foreach { connection =>
//     connection handleWithSyncHandler requestHandler
//   }).
//     run()

//   println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")

//   StdIn.readLine() // let it run until user presses return

//   bindingFuture
//     .flatMap(_.unbind()) // trigger unbinding from the port
//     .onComplete(_ => system.terminate()) // and shutdown when done

// }
