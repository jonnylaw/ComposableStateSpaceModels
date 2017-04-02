package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import breeze.linalg.{DenseVector, DenseMatrix, cholesky}
import breeze.numerics.{log, exp}
import breeze.stats.distributions.{Gamma, Gaussian}
import cats._
import cats.implicits._
import com.github.jonnylaw.model._
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._
import scala.concurrent._

trait TestNegBinMod {
  implicit val system = ActorSystem("LinearModel")
  implicit val materializer = ActorMaterializer()

  val sde = Sde.brownianMotion(1)
  val p = Parameters.leafParameter(Some(log(3.0)), 
    SdeParameter.brownianParameter(0.0, log(1.0), 0.3, log(0.5)))

  val model = Model.negativeBinomial(sde)
}

object NegBinSample extends App with TestNegBinMod {
  SimulateData(model(p)).
    observations.
    take(200).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/NegativeBinomial.csv")).
    onComplete(_ => system.terminate())
}

object FilterNegBin extends App with TestNegBinMod {
  val data = DataFromFile("data/NegativeBinomial.csv").observations

  val t0 = 0.0
  val filter = ParticleFilter.filter(Resampling.systematicResampling _, t0, 1000)

  data.
    via(filter(model(p))).
    map(ParticleFilter.getIntervals(model(p))).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/NegativebinomialFiltered.csv")).
    onComplete(_ => system.terminate())
}

object NegBinPilotRun extends App with TestNegBinMod {
  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[State, Id] = Resampling.systematicResampling _

  val res = for {
    data <- DataFromFile("data/NegativeBinomial.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun(data.toVector, model, p, resample, particles, 100)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/NegBinPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())

}

object InitialNegBinPosterior extends App with TestNegBinMod with DataProtocols {
  val resample: Resample[State, Id] = Resampling.systematicResampling _

  def prior = (p: Parameters) => p match {
    case LeafParameter(Some(scale), BrownianParameter(m, c, mu, s)) =>
      Gamma(3.0, 1.0).logPdf(exp(scale)) +
      Gaussian(0.0, 1.0).logPdf(m) +
      Gamma(1.0, 1.0).logPdf(exp(c)) +
      Gaussian(0.3, 1.0).logPdf(mu) +
      Gamma(0.5, 1.0).logPdf(exp(s))
  }

  def simPrior = for {
    scale <- Gamma(3.0, 1.0)
    m <- Gaussian(0.0, 1.0)
    c <- Gamma(1.0, 1.0)
    mu <- Gaussian(0.3, 1.0)
    s <- Gamma(0.5, 1.0)
  } yield Parameters.leafParameter(Some(log(scale)), SdeParameter.brownianParameter(m, log(c), mu, log(s)))

  DataFromFile("data/NegativeBinomial.csv").
    observations.
    take(200).
    grouped(200).
    mapConcat(d => (1 to 2).map((d, _))).
    mapAsync(2) { case (d, chain) =>
      val filter = ParticleFilter.llStateReader(d.toVector, resample, 100)
      val pf = (filter compose model)
      val pmmh = MetropolisHastings.pmmhState(simPrior.draw, Parameters.perturb(0.05), prior)

      pmmh(pf).
        take(10000).
        via(Streaming.monitorStateStream).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile(s"data/NegBinPosterior-$chain.json"))
    }.
    runWith(Sink.onComplete(_ => system.terminate()))
}

object NegBinPosterior extends App with TestNegBinMod with DataProtocols {
  val resample: Resample[State, Id] = Resampling.systematicResampling _

  def prior = (p: Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(scale), BrownianParameter(m, c, mu, s)) =>
      Gamma(3.0, 1.0).logPdf(exp(scale)) +
      Gaussian(0.0, 1.0).logPdf(m) +
      Gamma(1.0, 1.0).logPdf(exp(c)) +
      Gaussian(0.3, 1.0).logPdf(mu) +
      Gamma(0.5, 1.0).logPdf(exp(s))
  }

  def simPrior = for {
    scale <- Gamma(3.0, 1.0)
    m <- Gaussian(0.0, 1.0)
    c <- Gamma(1.0, 1.0)
    mu <- Gaussian(0.3, 1.0)
    s <- Gamma(0.5, 1.0)
  } yield Parameters.leafParameter(Some(log(scale)), SdeParameter.brownianParameter(m, log(c), mu, log(s)))

  def posterior(files: List[String]): Future[List[(DenseMatrix[Double], Parameters)]] = Future.sequence(
    files.
      map(f => {
        for {
          params <- Streaming.readParamPosterior(f, 0, 1).map(_.params).runWith(Sink.seq)
        } yield (Parameters.covariance(params), params.last)
      })
  )

  val scale = 0.5
  val files = (1 to 2).map(i => s"data/NegBinPosterior-$i.json").toList

  DataFromFile("data/NegativeBinomial.csv").
    observations.
    take(200).
    grouped(200).
    zip(Source.fromFuture(posterior(files))).
    mapConcat { case (d, post) => post.zipWithIndex.map { case (p, chain) => (d, p, chain) }}.
    mapAsync(2) { case (d, post, chain) =>
      val filter = ParticleFilter.llStateReader(d.toVector, resample, 100)
      val pf = (filter compose model)
      val pmmh = MetropolisHastings.pmmhState(post._2, Parameters.perturbMvn(cholesky(scale * scale * post._1)), prior)

      pmmh(pf).
        take(10000).
        via(Streaming.monitorStateStream).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile(s"data/NegBinPosterior-1-$chain.json"))
    }.
    runWith(Sink.onComplete(_ => system.terminate()))
}
