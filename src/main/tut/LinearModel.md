This is a simple example of 


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
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(0.5),
      c0 = DenseVector(0.12),
      theta = DenseVector(3.0),
      alpha = DenseVector(0.2),
      sigma = DenseVector(0.5))
  )

  val mod = Model.linearModel(Sde.ornsteinUhlenbeck)

  def prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(v), OrnsteinParameter(m, c, theta, alpha, sigma)) =>
      Gamma(0.2, 5.0).logPdf(v) + 
      Gaussian(0.5, 2.0).logPdf(m(0)) +
      Gamma(0.02, 5.0).logPdf(c(0)) + 
      Gaussian(0.01, 2.0).logPdf(theta(0)) +
      Gamma(0.05, 2.5).logPdf(alpha(0)) +
      Gamma(0.2, 2.5).logPdf(sigma(0))
  }

  def simPrior = for {
    v <- Gamma(0.2, 5.0)
    m <- Gaussian(0.5, 2.0)
    c <- Gamma(0.02, 5.0)
    t <- Gaussian(0.01, 2.0)
    a <- Gamma(0.05, 2.5)
    s <- Gamma(0.2, 2.5)
  } yield Parameters.leafParameter(Some(v),
    SdeParameter.ornsteinParameter(
      DenseVector(m),
      DenseVector(c),
      DenseVector(t),
      DenseVector(a),
      DenseVector(s)))


  def perturb = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(v), OrnsteinParameter(m, c, t, a, s)) =>
      for {
        innov_v <- Gaussian(0.0, 0.1)
        new_v = v * exp(innov_v)
        new_m <- Gaussian(m(0), 0.1)
        innov_c <- Gaussian(0.0, 0.025)
        new_c = c :* exp(innov_c)
        new_t <- Gaussian(t(0), 0.05)
        innov_a <- Gaussian(0.0, 0.25)
        new_a = a :* exp(innov_a)
        innov_s <- Gaussian(0.0, 0.025)
        new_s = s :* exp(innov_s)
      } yield Parameters.leafParameter(
        Some(new_v),
        SdeParameter.ornsteinParameter(DenseVector(new_m), new_c, DenseVector(new_t), new_a, new_s))
  }

  implicit val system = ActorSystem("LinearModel")
  implicit val materializer = ActorMaterializer()
}

object SimLinearModel extends App with LinearTestModel {
  SimulateData(mod(linearParam)).
    simRegular(1.0).
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
    Resampling.treeSystematicResampling _, t0, 1000)

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
  val resample: Resample[State, Id] = Resampling.treeSystematicResampling _

  val res = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun(data.toVector, mod, linearParam, resample, particles)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/LinearPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

object DetermineLinearParameters extends App with LinearTestModel {
  def iters(chain: Int, initParams: Parameters): Future[IOResult] = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.take(400).runWith(Sink.seq)
    filter = (p: Parameters) => ParticleFilter.likelihood(data.toVector, Resampling.treeStratifiedResampling _, 150)(mod(p))
    pmmh = ParticleMetropolisSerial(filter, initParams, perturb, prior)
    io <- pmmh.
      params.
      take(10000).
      via(Streaming.monitorStream).
      map(_.show).
      runWith(Streaming.writeStreamToFile(s"data/LinearModelParams-$chain.csv"))
    } yield io

  Future.sequence(simPrior.sample(2).zipWithIndex.map { case (p, i) => iters(i, p) }).
    onComplete(_ => system.terminate())
}

object LinearFullPosterior extends App with LinearTestModel {
  val serialization = SerializationExtension(system)
 
  val res: Future[Seq[MetropState]] = for {
    data <- DataFromFile("data/LinearModelSims.csv").observations.take(400).runWith(Sink.seq)
    filter = (p: Parameters) => ParticleFilter.filterLlState(data.toVector, Resampling.treeSystematicResampling _, 150)(mod(p))
    pmmh = ParticleMetropolisState(filter, simPrior.draw, Parameters.perturb(0.05), prior)
    iters <- pmmh.
      iters.
      zip(Source(Stream.from(1))).
      map { case (s, i) => {
        if (i % 100 == 0) {
          println(s"accepted: ${s.accepted}, iteration: $i")
          s
        } else {
          s
        }
      }}.
      take(10000).
      runWith(Sink.seq)
  } yield iters

  res.
    map(i => Streaming.serialiseToFile(i, "data/LinearModelPosterior")).
    onComplete(_ => system.terminate())
}

/**
  * Perform a one step forecast of the data
  */
object OneStepForecastLinear extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations

  val filter = (p: Parameters) => 
    ParticleFilter.filter(Resampling.treeSystematicResampling _, 0.0, 1000)(mod(p))

  // set the prediction interval
  val dt = 1.0

  data.
    via(filter(linearParam)).
    map(s => ParticleFilter.getMeanForecast(s, mod(linearParam), s.t + dt)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearModelForecast.csv")).
    onComplete(_ => system.terminate())
}

object LinearOnlineFilter extends App with LinearTestModel {
  val data = DataFromFile("data/LinearModelSims.csv").
    observations

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

  // the time of the last observation
  val t0 = 400.0

  def resample: Resample[(Parameters, State), Id] = Resampling.treeSystematicResampling _

  // Run the filter
  def filter = ParticleFilter.filterFromPosterior(resample, 
    simPosterior, 5000, t0)(mod.run)
    
  data.
    drop(400).
    take(100).
    via(filter).
    map(s => PfState(s.t, s.observation, s.ps.map(_._2), s.ll, 0)). 
    map(ParticleFilter.getIntervals(mod(linearParam))).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearOnlineFilter.csv")).
    onComplete(_ => system.terminate())
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
    map(_.sample(10).toVector).
    map(SimulateData.summariseForecast(mod(linearParam), 0.99)).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/LinearLongForecast.csv")).
    onComplete(_ => system.terminate())
}
