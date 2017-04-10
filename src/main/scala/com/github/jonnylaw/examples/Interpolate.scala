package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure

object Interpolate extends App with TestNegBinMod {
  implicit val system = ActorSystem("Interpolation")
  implicit val materializer = ActorMaterializer()

  val testData = DataFromFile("data/NegativeBinomial.csv").
    observations.
    drop(4000)

  // set up the particle filter
  val t0 = 4000 * 0.1
  val resample: Resample[State] = Resampling.systematicResampling _
  val filter = ParticleFilter.filter(resample, t0, 1000) compose model

  // remove some observations systematically
  testData.
    map { 
      case d: TimedObservation => if (d.t < 420 || d.t > 450) { d } else { d.copy(observation = None) }
      case _ => throw new Exception("Incorrect Data Format")
    }.
    map((d: Data) => d).
    via(filter(params)).
    map(ParticleFilter.getIntervals(model(params))).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/NegativeBinomialInterpolated.csv")).
    onComplete { s => 
      println(s)
      system.terminate()
    }
}
