package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.DenseVector
import breeze.stats.distributions.{Gaussian, Gamma}
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import java.io.File
import scala.concurrent.ExecutionContext.Implicits.global

trait LgcpModel {
  /** Define the model **/
  val params = Parameters.leafParameter(
    None,
    OrnsteinParameter(DenseVector(2.0),
      DenseVector(1.0),
      theta = DenseVector(0.1),
      alpha = DenseVector(0.4),
      sigma = DenseVector(0.5)))

  val model = Model.lgcpModel(Sde.ornsteinUhlenbeck)

  implicit val system = ActorSystem("LgcpModel")
  implicit val materializer = ActorMaterializer()
}

object SimulateLGCP extends App with LgcpModel {
  Source(SimulateData(model(params)).
    simLGCP(0.0, 10.0, 1)).
    map((d: Data) => d.show).
    runWith(Streaming.writeStreamToFile("data/lgcpsims.csv")).
    onComplete(_ => system.terminate())
}

object FilteringLgcp extends App with LgcpModel {
  val pf = FilterLgcp(model(params), Resampling.parMultinomialResampling, 3)

  // read from file and order the readings
  val data = DataFromFile("data/lgcpsims.csv").observations.runWith(Sink.seq)

  data.flatMap(d => Source(d.sortBy(_.t)).
    via(pf.filterStream(0.0)(1000)).
    map(ParticleFilter.getIntervals(model(params))).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/lgcpfiltered.csv"))).
    onComplete(_ => system.terminate())
}

object GetLgcpParams extends App with LgcpModel {
  // choose a prior
  def prior = (p: Parameters) => p match {
    case LeafParameter(None, OrnsteinParameter(m, c, t, a, s)) =>
      Gaussian(0.0, 10.0).logPdf(m(0)) + 
      Gamma(1.0, 1.0).logPdf(c(0)) +
      Gaussian(2.0, 10.0).logPdf(t(0)) +
      Gamma(2.0, 1.0).logPdf(a(0)) +
      Gamma(1.0, 1.0).logPdf(s(0))
    case _ => throw new Exception
  }

  // the higher this value, the higher the accuracy of the calculated cumulative hazard 
  val precision = 2

  // read in the LGCP simulated data using akka
  val res = for {
    data <- DataFromFile("data/lgcpsims.csv").observations.runWith(Sink.seq)
    filter = Reader { (mod: Model) => FilterLgcp(mod, Resampling.parMultinomialResampling, precision).
      llFilter(data.toVector)(200)}
    mll = filter compose model
    pmmh = ParticleMetropolis(mll.run, params, Parameters.perturb(0.05), prior)
    fileio <- pmmh.params.take(10000).map(_.show).runWith(Streaming.writeStreamToFile("data/lgcpMCMC.csv"))
  } yield fileio


  res.onComplete(_ => system.terminate())
}
