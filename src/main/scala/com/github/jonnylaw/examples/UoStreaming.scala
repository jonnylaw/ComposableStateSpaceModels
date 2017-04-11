package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import HttpEntity.ChunkStreamPart
import Uri.Query
import HttpMethods._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._
import com.github.jonnylaw.model._
import spray.json._
import akka.http.scaladsl.common.{EntityStreamingSupport, JsonEntityStreamingSupport}
import DataProtocols._


object UoClient extends App {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher
  implicit val jsonStreamingSupport: JsonEntityStreamingSupport = EntityStreamingSupport.json()

}
