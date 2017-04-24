package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._
import scala.concurrent.ExecutionContext.Implicits.global

object Interpolate extends App with TestNegBinMod {
  implicit val system = ActorSystem("Interpolation")
  implicit val materializer = ActorMaterializer()

  val testData = DataFromFile("data/NegativeBinomial.csv").
    observations.
    drop(4000)

  // set up the particle filter
  val t0 = 4000 * 0.1
  val resample: Resample[List[State]] = Resampling.systematicResampling _
  val filter = ParticleFilter.interpolate(resample, t0, 1000) compose model

  // remove some observations systematically
  val raw_data = testData.
    map { 
      case d: TimedObservation => if (d.t < 420 || d.t > 450) { d } else { d.copy(observation = None) }
      case _ => throw new Exception("Incorrect Data Format")
    }.
    map((d: Data) => d).
    via(filter(params)).
    runWith(Sink.seq)

  // the particle filter aggregates at each step a copy of the entire ancestral lineage of the path
  val res = raw_data.
    map {
      s => (s, s.last.particles.transpose).zipped.map { case (x, p) =>  PfState(x.t, x.observation, p, x.ll, x.ess) }.
        map(ParticleFilter.getIntervals(model, params).run)
    }

  Source.fromFuture(res).
    mapConcat(identity).
    map((s: PfOut) => s.show).
    runWith(Streaming.writeStreamToFile("data/NegativeBinomialInterpolated.csv")).
    onComplete { s =>
      println(s)
      system.terminate()
    }
}
