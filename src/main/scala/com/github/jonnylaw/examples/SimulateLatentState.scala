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

  val p = SdeParameter.brownianParameter(
    DenseVector.fill(2)(1.0),
    DenseVector.fill(2)(log(1.0)),
    DenseVector.fill(2)(-0.3),
    DenseVector.fill(2)(log(0.3)))

  val p1 = SdeParameter.brownianParameter(
    DenseVector.fill(2)(1.0),
    DenseVector.fill(2)(log(1.0)),
    DenseVector.fill(2)(0.3),
    DenseVector.fill(2)(log(0.3)))

  val sde1 = Sde.brownianMotion(p)
  val sde2 = Sde.brownianMotion(p1)

  val composedSde = sde1 |+| sde2

  composedSde.
    simStream(0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/brownianMotion.csv"))).
    onComplete(_ => system.terminate())
}

object SimOrnstein extends App {
  implicit val system = ActorSystem("LatentState")
  implicit val materializer = ActorMaterializer()

  val p = SdeParameter.ornsteinParameter(
    DenseVector.fill(2)(0.0),
    DenseVector.fill(2)(log(3.0)),
    theta = DenseVector(2.0, 1.0), 
    alpha = DenseVector.fill(2)(log(0.5)),
    sigma = DenseVector.fill(2)(log(0.3)))

  val sde = Sde.ornsteinUhlenbeck

  sde(p).
    simStream(0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/ornsteinUhlenbeck.csv"))).
    onComplete(_ => system.terminate()) 
}
