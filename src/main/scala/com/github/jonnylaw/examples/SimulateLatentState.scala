package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.stats.distributions.{Gaussian, MultivariateGaussian, Gamma}
import breeze.linalg.{DenseVector, diag}
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Simulate an SDE
  */
object SimulateBrownianMotion extends App {
  implicit val system = ActorSystem("LatentState")
  implicit val materializer = ActorMaterializer()

  val p = SdeParameter.genBrownianParameter(1.0, log(1.0), -0.3, log(0.3))

  val p1 = SdeParameter.genBrownianParameter(1.0, log(1.0), 0.3, log(0.3))

  val sde1 = Sde.genBrownianMotion(2)(p)
  val sde2 = Sde.genBrownianMotion(2)(p1)

  val composedSde = sde1 |+| sde2

  composedSde.
    simStream(0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    runWith(Streaming.writeStreamToFile("data/brownianMotion.csv")).
    onComplete(_ => system.terminate())
}

object SimOrnstein extends App {
  implicit val system = ActorSystem("LatentState")
  implicit val materializer = ActorMaterializer()

  val p = SdeParameter.ouParameter(0.0, log(3.0), log(0.5), log(0.3))(2.0)

  val sde = Sde.ouProcess(2)

  sde(p).
    simStream(0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    runWith(Streaming.writeStreamToFile("data/ornsteinUhlenbeck.csv")).
    onComplete(_ => system.terminate()) 
}
