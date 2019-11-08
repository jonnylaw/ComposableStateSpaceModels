package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString
import breeze.numerics.log
import cats.implicits._
import com.github.jonnylaw.model._

object Interpolate extends App with TestModel {
  implicit val system = ActorSystem("Interpolation")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val testData = DataFromFile(s"data/$modelName.csv").
    observations.
    drop(4000)

  val res = for {
    // get the starting time of the stream
    t0 <- testData.map(_.t).runWith(Streaming.minSink)

    // set up the particle filter
    resample: Resample[List[State]] = Resampling.systematicResampling _
    filter = ParticleFilter.interpolate(resample, t0, 1000) compose model

    // remove some observations systematically, setting the observation to None
    interpolated <- testData.
      map {
        case d: TimedObservation => if (d.t < 420 || d.t > 450) { d } else { d.copy(observation = None) }
        case _ => throw new Exception("Incorrect Data Format")
      }.
      map((d: Data) => d).
      via(filter(params)).
      runWith(Sink.seq)

    summary = (interpolated, interpolated.last.particles.transpose).zipped.
      map { case (x, p) =>  PfState(x.t, x.observation, p, x.ll, x.ess) }.
      map(ParticleFilter.getIntervals(model, params))

    io <- Source(summary).
      map((s: PfOut[State]) => s.show).
      runWith(Streaming.writeStreamToFile(s"data/${modelName}Interpolated.csv"))
    } yield io

  res.
    onComplete { s =>
      println(s)
      system.terminate()
    }
}
