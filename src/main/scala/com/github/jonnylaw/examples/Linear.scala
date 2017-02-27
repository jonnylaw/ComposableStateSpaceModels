package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.serialization._
import akka.stream._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats._
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import java.io._
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait LinearTestModel {
  val linearParam: Parameters = Parameters.leafParameter(
    Some(1.0),
    SdeParameter.brownianParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      mu = DenseVector(0.01),
      sigma = DenseVector(0.5))
  )

  val mod = Model.linearModel(Sde.brownianMotion)

  def prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(v), BrownianParameter(m, c, mu, sigma)) =>
      Gamma(0.2, 5.0).logPdf(v) + 
      Gaussian(0.5, 2.0).logPdf(m(0)) +
      Gamma(0.2, 5.0).logPdf(c(0)) + 
      Gaussian(0.01, 2.0).logPdf(mu(0)) +
      Gamma(0.2, 2.5).logPdf(sigma(0))
  }

  implicit val system = ActorSystem("LinearModel")
  implicit val materializer = ActorMaterializer()
}

object SimLinearModel extends App with LinearTestModel {
  SimulateData(mod(linearParam)).
    observations.
    take(500).
    map((d: Data) => d.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/LinearModelSims.csv"))).
    onComplete(_ => system.terminate())
}

object FilterLinear extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations
    
  val t0 = 0.0

  val filter = ParticleFilter.filter[Vector](
    Resampling.treeSystematicResampling, t0, 1000)

  data.
    via(filter(mod(linearParam))).
    map(ParticleFilter.getIntervals(mod(linearParam))).
    map(_.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/LinearModelFiltered.csv"))).
    onComplete(_ => system.terminate())
}

/**
  * Perform a pilot run of the PMMH algorithm, to determine the optimum number of particles to use in the particle filter
  */
object PilotRunLinear extends App with LinearTestModel {
  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[Vector, Id, State] = Resampling.treeSystematicResampling _

  val res = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun[Vector](data.toVector, mod, linearParam, resample, particles)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/LinearPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

object DetermineLinearParameters extends App with LinearTestModel {
  def iters(chain: Int): Future[IOResult] = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.take(400).runWith(Sink.seq)
    filter = ParticleFilter.likelihood[Vector](data.toVector,
        Resampling.treeStratifiedResampling, 150).compose(mod)
    pmmh = ParticleMetropolis(filter.run, linearParam, Parameters.perturb(0.05), prior)
    io <- pmmh.
        params.
        take(10000).
        map(_.show).
        runWith(Streaming.writeStreamToFile(s"data/LinearModelParams-$chain.csv"))
  } yield io

  Future.sequence((1 to 2).map(iters)).
    onComplete(_ => system.terminate())
}

object LinearFullPosterior extends App with LinearTestModel {
  val serialization = SerializationExtension(system)
 
  val res = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.take(400).runWith(Sink.seq)
    filter = ParticleFilter.filterLlState[Vector](data.toVector,
        Resampling.treeSystematicResampling, 150).compose(mod)
    pmmh = ParticleMetropolisState(filter.run, linearParam, Parameters.perturb(0.05), prior)
    io <- pmmh.
    itersStream.
    take(10000).
    map(state => {
      val serializer = serialization.findSerializerFor(state)
      serializer.toBinary(state)
    }).
    map(s => ByteString(s)).
    runWith(StreamConverters.fromOutputStream(() => new FileOutputStream("data/LinearModelPosterior")))
  } yield io

  res.onComplete(_ => system.terminate())
}

// object FilterLinear extends App with LinearTestModel {
//   val posterior: Vector[MetropState] = deserialise(new FileInputStream("data/LinearModelPosterior")) match {
//     case p: Vector[MetropState] => p
//   }

//   val ps = Resampling.sampleMany(100, posterior)
  
// }

/**
  * Perform a one step forecast of the data
  */
object OneStepForecastLinear extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations

  val filter = ParticleFilter.filter[ParVector](Resampling.parMultinomialResampling, 0.0, 1000).
    compose(mod)

  // set the prediction interval, predict five minutes ahead from each observation
  val dt = 5.0/60

  data.
    via(filter(linearParam)).
    map(s => ParticleFilter.getMeanForecast(s, mod(linearParam), s.t + dt)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearModelForecast.csv")).
    onComplete(_ => system.terminate())
}
