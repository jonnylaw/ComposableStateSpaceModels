package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import java.nio.file.Paths
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait SeasonalTestModel {
  val seasonalParam: Parameters = Parameters.leafParameter(
    Some(3.0),
    SdeParameter.brownianParameter(
      m0 = DenseVector.fill(12)(0.5),
      c0 = DenseVector.fill(12)(0.12),
      mu = DenseVector.fill(12)(0.1),
      sigma = DenseVector.fill(12)(0.5))
  )

  val mod = Model.seasonalModel(24, 6, Sde.brownianMotion)

  implicit val system = ActorSystem("SeasonalModel")
  implicit val materializer = ActorMaterializer()
}

object SimSeasonalModel extends App with SeasonalTestModel {
  SimulateData(mod(seasonalParam)).
    simRegular(1.0).
    take(500).
    map((d: Data) => d.show).
    runWith(Streaming.writeStreamToFile("data/SeasonalModelSims.csv")).
    onComplete(_ => system.terminate())
}

object FilterSeasonal extends App with SeasonalTestModel {
  val data = DataFromFile("data/SeasonalModelSims.csv").
    observations

  val t0 = 0.0

  val filter = ParticleFilter.filter[Vector](Resampling.treeSystematicResampling, t0, 1000)

  data.
    via(filter(mod(seasonalParam))).
    map(ParticleFilter.getIntervals(mod(seasonalParam))).
    drop(1).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/SeasonalModelFiltered.csv")).
    onComplete(_ => system.terminate())
}

object DetermineSeasonalParameters extends App with SeasonalTestModel {
  def prior(p: Parameters) = { p match {
    case LeafParameter(Some(v), BrownianParameter(m, c, mu, sigma)) =>
      Gamma(0.5, 1.0).logPdf(v) +
      m.mapValues(Gaussian(0.5, 3.0).logPdf(_)).reduce(_+_) + 
      c.mapValues(Gamma(0.05, 2.0).logPdf(_)).reduce(_+_) + 
      mu.mapValues(Gaussian(0.1, 3.0).logPdf(_)).reduce(_+_) + 
      sigma.mapValues(Gamma(0.2, 3.0).logPdf(_)).reduce(_+_)
  }}


  def iters(chain: Int): Future[IOResult] = for {
    data <- DataFromFile("data/SeasonalModelSims.csv").observations.take(400).runWith(Sink.seq)
    filter = ParticleFilter.likelihood[Vector](data.toVector,
        Resampling.treeStratifiedResampling, 150).compose(mod)
    pmmh = ParticleMetropolis(filter.run, seasonalParam, Parameters.perturb(0.05), prior)
    io <- pmmh.
        params.
        take(10000).
        map(_.show).
        runWith(Streaming.writeStreamToFile(s"data/SeasonalModelParams-$chain.csv"))
  } yield io

  Future.sequence((1 to 2).map(iters)).
    onComplete(_ => system.terminate())
}
