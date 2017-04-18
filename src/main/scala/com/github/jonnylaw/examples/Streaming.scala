package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import akka.NotUsed
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import HttpEntity.ChunkStreamPart
import Uri.Query
import HttpMethods._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._

import cats.data.Reader
import cats.implicits._

import com.github.jonnylaw.model._
import scala.io.StdIn
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import spray.json._
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import com.github.jonnylaw.model.DataProtocols._

/**
  * Serve the data as a stream of JSON
  */
object FlatServer extends App with TestNegBinMod {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()

  val route = path("sims") {
    val simStream = SimulateData(model(params).right.get).
      observations.
      take(10)

    complete(simStream)
  }

  // serve the application on port 8080
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")

  StdIn.readLine() // let it run until user presses return

  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}

object StreamServer extends App with TestNegBinMod {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val start = ByteString.empty
  val sep = ByteString("\n")
  val end = ByteString.empty

  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json().
    withFramingRenderer(Flow[ByteString].intersperse(start, sep, end))

  val route = path("sims") {
    val simStream = SimulateData(model(params).right.get).
      observations.
      zip(Source.tick(1.second, 1.second, ())).
      map(_._1)

    complete(simStream)
  }

  // serve the application on port 8080
  val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

  println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")

  StdIn.readLine() // let it run until user presses return

  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}

object Client extends App with TestNegBinMod {
  implicit val system = ActorSystem("Client")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val filter = ParticleFilter.filter(Resampling.multinomialResampling, 0.0, 100).lift[Error] compose model

  val res: Future[HttpResponse] = 
    Http().singleRequest(HttpRequest(GET, uri = "http://localhost:8080/sims"))

  res andThen {
    case Success(response) => {
      val res = response.
        entity.
        dataBytes.
        map(_.utf8String).
        map(_.parseJson.convertTo[TimedObservation]).
        runWith(Sink.seq)

      res.onComplete { 
        case Success(readings) => readings.foreach(println)
        case Failure(ex) => println(s"damn, $ex")
      }
    }
    case Failure(ex) => println(s"oh man, $ex")
  } onComplete { s =>
    println(s)
    system.terminate()
  }
}
