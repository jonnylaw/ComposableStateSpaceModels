package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.serialization._
import akka.stream._
import akka.NotUsed
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.linalg.cholesky
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats._
import cats.data.{Reader, Kleisli}
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import java.io._
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import spray.json._

trait LinearTestModel {
  val linearParam: Parameters = Parameters.leafParameter(
    Some(log(1.0)),
    SdeParameter.ouParameter(m0 = 0.5, c0 = log(0.12), theta = 3.0, alpha = log(0.2), sigma = log(0.5)))

  val mod = Model.linearModel(Sde.ouProcess(1))

  def prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(v), OuParameter(m, c, theta, alpha, sigma)) =>
      Gamma(0.2, 5.0).logPdf(exp(v)) + 
      Gaussian(0.5, 2.0).logPdf(m) +
      Gamma(0.02, 5.0).logPdf(exp(c)) + 
      Gaussian(3.0, 2.0).logPdf(theta) +
      Gamma(0.05, 2.5).logPdf(exp(alpha)) +
      Gamma(0.2, 2.5).logPdf(exp(sigma))
  }

  def simPrior = for {
    v <- Gamma(0.2, 5.0)
    m <- Gaussian(0.5, 2.0)
    c <- Gamma(0.02, 5.0)
    t <- Gaussian(3.0, 2.0)
    a <- Gamma(0.05, 2.0)
    s <- Gamma(0.2, 2.5)
  } yield Parameters.leafParameter(Some(log(v)),
    SdeParameter.ouParameter(m, log(c), t, log(a), log(s)))

  implicit val system = ActorSystem("LinearModel")
  implicit val materializer = ActorMaterializer()
}

object SimLinearModel extends App with LinearTestModel {
  SimulateData(mod(linearParam)).
    simRegular(0.05).
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

  val filter = ParticleFilter.filter(
    Resampling.systematicResampling _, t0, 1000)

  data.
    via(filter(mod(linearParam))).
    map(ParticleFilter.getIntervals(mod(linearParam))).
    map(_.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/LinearModelFiltered.csv"))).
    onComplete(_ => system.terminate())
}

object FilterDsl extends App with LinearTestModel with DataProtocols {
  val resample: Resample[State, Id] = Resampling.systematicResampling _

  DataFromFile("data/LinearModelSims.csv").
    observations.
    take(400).
    grouped(400).
    mapAsync(1) { d =>
      val filter = ParticleFilter.llStateReader(d.toVector, resample, 1000)
      val pf = (filter compose mod)
      val pmmh = MetropolisHastings.pmmhState(simPrior.draw, Parameters.perturb(0.05), prior)

      pmmh(pf).
        take(1000).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile("data/LinearPosterior.csv"))
    }.
    runWith(Sink.onComplete(_ => system.terminate()))
}

/**
  * Perform a pilot run of the PMMH algorithm, to determine the optimum number of particles to use in the particle filter
  */
object PilotRunLinear extends App with LinearTestModel {
  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[State, Id] = Resampling.systematicResampling _

  val res = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun(data.toVector, mod, linearParam, resample, particles)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/LinearPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

/**
  * Determine the full joint posterior of the state and the parameters of the Linear Model
  * then serialise the results to JSON, so it can be read in and used for the online filtering
  */
object InitialLinearFullPosterior extends App with LinearTestModel with DataProtocols {
  DataFromFile("data/LinearModelSims.csv").
    observations.
    take(400).
    grouped(400).
    mapConcat(s => (1 to 2).map((s, _))).
    mapAsync(2) { case (data, chain) => {
      val filter = (p: Parameters) => ParticleFilter.filterLlState(data.toVector, 
        Resampling.systematicResampling _, 200)(mod(p))
      val pmmh = ParticleMetropolisState(filter, linearParam, Parameters.perturb(0.05), prior)
      pmmh.
        iters.
        via(Streaming.monitorStateStream).
        take(10000).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile(s"data/LinearModelPosterior-$chain.json"))
    }}.
    runWith(Sink.onComplete(_ => system.terminate()))
}

/**
  * Once a pilot run of the PMMH algorithm has been completed, the covariance of the posterior can be used
  * as the covariance of the proposal distribution
  */
object MvnLinearFullPosterior extends App with LinearTestModel with DataProtocols {
  val files = List("data/LinearModelPosterior-1.json","data/LinearModelPosterior-2.json")

  def posterior(files: List[String]): Future[List[(DenseMatrix[Double], Parameters)]] = Future.sequence(
    files.
      map(f => {
        for {
          params <- Streaming.readParamPosterior(f, 0, 1).map(_.params).runWith(Sink.seq)
        } yield (Parameters.covariance(params), params.last)
      })
  )

  val scale = 0.25

  DataFromFile("data/LinearModelSims.csv").
    observations.
    take(400).
    grouped(400).
    zip(Source.fromFuture(posterior(files))).
    mapConcat { case (d, post) => post.zipWithIndex.map { case (p, chain) => (d, p, chain) } }.
    mapAsync(2) { case (data, post, chain) => {
      val filter = (p: Parameters) => ParticleFilter.filterLlState(data.toVector, Resampling.systematicResampling _, 200)(mod(p))
      val pmmh = ParticleMetropolisState(filter, post._2, Parameters.perturbMvn(cholesky(scale * scale * post._1)), prior)
      pmmh.
        iters.
        via(Streaming.monitorStateStream).
        take(10000).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile(s"data/LinearModelPosterior-final$chain.json"))
    }}.
    runWith(Sink.onComplete(s => {
      println(s)
      system.terminate()
    }))
}

/**
  * Perform a one step forecast of the data
  */
object OneStepForecastLinear extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations

  val filter = (p: Parameters) => 
    ParticleFilter.filter(Resampling.systematicResampling _, 0.0, 1000)(mod(p))

  // set the prediction interval
  val dt = 1.0

  data.
    via(filter(linearParam)).
    map(s => ParticleFilter.getMeanForecast(s, mod(linearParam), s.t + dt)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearModelForecast.csv")).
    onComplete(_ => system.terminate())
}

// object LinearOnlineFilter extends App with LinearTestModel with DataProtocols {
//   val data = DataFromFile("data/LinearModelSims.csv").
//     observations

//   val simPosterior = for {
//     posterior <- Streaming.readPosterior("data/LinearModelPosterior.json", 1000, 2).runWith(Sink.seq)
//     simPosterior = Streaming.createDist(posterior)(x => (x.params, x.sde.state))
//   } yield simPosterior

//   // the time of the last observation
//   val t0 = 400.0

//   def resample: Resample[(Parameters, State), Id] = Resampling.SystematicResampling _

//   simPosterior.flatMap((post: Rand[(Parameters, State)]) => {
//     def filter = ParticleFilter.filterFromPosterior(resample, post, 5000, t0)(mod.run)

//     data.
//       drop(400).
//       take(100).
//       via(filter).
//       map(s => PfState(s.t, s.observation, s.ps.map(_._2), s.ll, 0)).
//       map(ParticleFilter.getIntervals(mod(linearParam))).
//       map(_.show).
//       runWith(Streaming.writeStreamToFile("data/LinearOnlineFilter.csv"))
//   }).onComplete(_ => system.terminate())
// }

/**
  * Run 100 particle filters by sampling 100 times from p(x, theta | y), each filter has M particles
  */
object LinearOnlineFilter extends App with LinearTestModel with DataProtocols {
  val testData = DataFromFile("data/LinearModelSims.csv").
    observations.
    drop(400).
    take(100)

  val t0 = 400 * 0.05

  def filterOnline(particles: Int)(post: (Parameters, State)): Source[PfState, Future[IOResult]] = {
    testData.
      take(100).
      via(ParticleFilter.filterInit(Resampling.systematicResampling, t0, particles, post._2)(mod(post._1)))
  }

  val simPosterior = for {
    posterior <- Streaming.readPosterior("data/LinearModelPosterior-1.json", 1000, 2).runWith(Sink.seq)
    simPosterior = Streaming.createDist(posterior)(x => (x.params, x.sde.state))
  } yield simPosterior

  val nFilters = 100

  // perform 100 particle filters, each with 1,000 particles
  Source.fromFuture(simPosterior).
    flatMapConcat { posterior => 
      val samples = posterior.sample(nFilters).toVector
      Source(samples)
    }.
    map(filterOnline(1000)).async. // run the filters in another thread
    flatMapMerge(4, identity).
    groupBy(101, _.t). // group each observation by time
    reduce((a, b) => PfState(a.t, a.observation, a.particles ++ b.particles, b.ll, b.ess + a.ess)). // combine the particles
    mergeSubstreams.
    map(ParticleFilter.getIntervals(mod(linearParam))). // summarise the observations
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearOnlineFilter.csv")).
    onComplete(s => {
      println(s)
      system.terminate()
    })
}

object LinearForecast extends App with LinearTestModel {
  val posterior: Seq[MetropState] = Streaming.deserialiseFile("data/LinearModelPosterior") match {
    case p: Seq[MetropState @unchecked] => p
    case _ => throw new Exception
  }

  val simPosterior: Rand[(Parameters, State)] = new Rand[(Parameters, State)] {
    def draw = {
      val s = Resampling.sampleOne(posterior.toVector)
      (s.params, s.sde.state)
    }
  }

  val t0 = 400.0

  val times = Source(400.0 to 500.0 by 1.0)

  times.
    via(SimulateData.forecast(mod.run, t0)(simPosterior)).
    map(_.sample(1000).toVector).
    map(SimulateData.summariseForecast(mod.run, 0.99)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearLongForecast.csv")).
    onComplete(_ => system.terminate())
}
