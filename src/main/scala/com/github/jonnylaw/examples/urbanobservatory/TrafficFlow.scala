package com.github.jonnylaw.examples.urbanobservatory

import akka.stream.scaladsl._
import akka.stream._
import akka.util.ByteString
import java.nio.file.Paths
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import HttpEntity.ChunkStreamPart
import Uri.Query
import HttpMethods._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._
import com.github.jonnylaw.model._
import com.github.nscala_time.time.Imports._
import com.typesafe.config._
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.util.{Try, Success, Failure}
import UoSensors._
import spray.json._
import slick.jdbc.SQLiteProfile.api._
import TrafficDatabaseTables._

trait UoAPI {
  val raw_data_uri = Uri("http://uoweb1.ncl.ac.uk/api/v1/sensor/data/raw.json")
  val live_data_uri = Uri("http://uoweb1.ncl.ac.uk/api/v1/sensors/live.json")
  val api_key = "73q7975bvu81zux05e7m1jbtpn03sv6qlnw3k8vzkn6ioc7jyu3zzbigekuoa987e7nenxvebbbjmo5woq5wnhdp1g" // your api key here
}

/**
  * Read the traffic flow information from one sensor from the static API.  
  * Map the data to timestamped observation for use with the modelling API
  */
object TrafficFlowStatic extends App with UoAPI {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  
  val query: Query = Query("api_key" -> api_key,
    "sensor_name" -> "N05171T",
    "start_time" -> "20170201",
    "end_time" -> "20170202",
    "variable" -> "traffic flow")

  val res: Future[HttpResponse] = 
    Http().singleRequest(HttpRequest(GET, uri = raw_data_uri.withQuery(query)))

  Source.fromFuture(res).
    flatMapConcat(_.entity.dataBytes).
    mapConcat(_.utf8String.parseJson.convertTo[List[Sensor]]).
    mapConcat(s => s.data.trafficFlow.data.toList).
    map{ l => TimestampObservation(l._1, l._1.getMillis(), Some(l._2)) }.
    runForeach(println).
    onComplete(_ => system.terminate())
}

/**
  * Read Traffic Flow Data using the Urban Observatory live json streaming API
  * This doesn't allow us to specify a single sensor, but instead a variable name and returns the latest reading from
  * each sensor recording this information
  */
object TrafficFlowLive extends App with UoAPI {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val query: Query = Query("api_key" -> api_key, "variable" -> "traffic flow")

  val res: Future[HttpResponse] = 
    Http().singleRequest(HttpRequest(GET, uri = live_data_uri.withQuery(query)))

  Source.fromFuture(res).
    flatMapConcat(_.entity.dataBytes).
    mapConcat(_.utf8String.parseJson.convertTo[List[Sensor]]).
    mapConcat(s => s.data.trafficFlow.data.toList map ((s.name, _))).
    map{ case (name, l) =>
      (name, TimestampObservation(l._1, l._1.getMillis(), Some(l._2)))
    }.
    runForeach(println).
    onComplete(_ => system.terminate())
}

/**
  * Create tables if they don't exist and insert traffic sensor meta data 
  */
// TODO: Handle errors from the API, as bad requests are not encoded as bad requests
object TrafficFlowMetaData extends App with UoAPI {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  // read in the data from the API
  val query: Query = Query("api_key" -> api_key,
    "sensor_name" -> "N05171T",
    "start_time" -> "20170101",
    "end_time" -> "20170413",
    "variable" -> "traffic flow")

  val res: Future[HttpResponse] = 
    Http().singleRequest(HttpRequest(GET, uri = live_data_uri.withQuery(query)))

  println(live_data_uri.withQuery(query))

  val sensorReadings = Flow[Sensor].mapConcat(x => x.data.trafficFlow.data.map (a => (x.name, a._1, a._2)))

  val raw_sensors = Source.fromFuture(res).
    flatMapConcat(_.entity.dataBytes).
    mapConcat(_.utf8String.parseJson.convertTo[List[Sensor]])

  // specify the database to use, loading information using typesafe config
  val db = Database.forConfig("sqlite1")

  // create the database tables
  // change this to create if not null
  // val createTables = (sensors.schema ++ trafficFlow.schema).create
  // val setup = db.run(DBIO.seq(createTables))

  val si = raw_sensors.
    mapConcat(x => x.data.trafficFlow.data.keys.toList.
      map (a => (x.name, true, x.data.trafficFlow.meta.units, a))).
    runWith(Sink.seq).
    map(_.maxBy(_._4))

    si.flatMap(s => db.run(DBIO.seq(sensors += s))).
      onComplete { _ => system.terminate() }
}

/**
  * Insert The traffic flow data into the Database from the raw data feed
  * The data is only there for the current month, not sure why...
  */
object InsertTrafficFlowData extends App with UoAPI {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  // read in the data from the API
  val query: Query = Query("api_key" -> api_key,
    "sensor_name" -> "N05171T",
    "start_time" -> "20170401",
    "end_time" -> "20170413",
    "variable" -> "traffic flow")

  val res: Future[HttpResponse] = 
    Http().singleRequest(HttpRequest(GET, uri = raw_data_uri.withQuery(query)))

  val sensorReadings = Flow[Sensor].mapConcat(x => x.data.trafficFlow.data.map (a => (x.name, a._1, a._2)))

  val raw_sensors = Source.fromFuture(res).
    flatMapConcat(_.entity.dataBytes).
    mapConcat(_.utf8String.parseJson.convertTo[List[Sensor]])

  // specify the database to use, loading information using typesafe config
  val db = Database.forConfig("sqlite1")

  val writeSensorData = raw_sensors.
    via(sensorReadings).
    groupedWithin(100, FiniteDuration(10L, "seconds")).
    mapAsync(1)(a => db.run(DBIO.seq(trafficFlow ++= a))).
    runWith(Sink.fold(0.0)((acc, _) => acc + 100))

  writeSensorData.
  onComplete { s =>
    println(s"Written $s lines")
    system.terminate()
  }
}

/**
  * Perform parameter inference by reading past information from the database
  */
object PMMHFromDatabase extends App {

}

/**
  * Perform a filter online
  */
object FilterOnline extends App with UoAPI {

}
