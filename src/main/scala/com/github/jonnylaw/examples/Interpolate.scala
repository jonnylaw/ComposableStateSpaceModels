package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.data.Kleisli
import cats.implicits._
import com.github.jonnylaw.model._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._

object Interpolate extends App with TestNegBinMod with DataProtocols {

}
