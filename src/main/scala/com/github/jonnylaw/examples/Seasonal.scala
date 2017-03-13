package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import breeze.linalg.{DenseVector, DenseMatrix}
import breeze.stats.distributions.{Gamma, Gaussian, Rand}
import breeze.numerics.{exp, log}
import cats._
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
      m0 = DenseVector.fill(2)(0.5),
      c0 = DenseVector.fill(2)(0.12),
      mu = DenseVector.fill(2)(0.1),
      sigma = DenseVector.fill(2)(0.5))
  )

  val mod = Model.seasonalModel(24, 1, Sde.brownianMotion)

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

/** Filter from the start of the series with the parameters used to simulate the model **/
object FilterSeasonal extends App with SeasonalTestModel {
  val data = DataFromFile("data/SeasonalModelSims.csv").
    observations

  val t0 = 0.0

  val filter = ParticleFilter.filter(Resampling.treeSystematicResampling, t0, 1000)

  data.
    via(filter(mod(seasonalParam))).
    map(ParticleFilter.getIntervals(mod(seasonalParam))).
    drop(1).
    map(_.show).
    runWith(Streaming.writeStreamToFile("data/SeasonalModelFiltered.csv")).
    onComplete(_ => system.terminate())
}

/** Determine the appropriate amount of particles in the particle filter for the seasonal model **/
object SeasonalPilotRun extends App with SeasonalTestModel {
  val particles = Vector(100, 200, 500, 1000, 2000)
  val resample: Resample[State, Id] = Resampling.treeSystematicResampling _

  val res = for {
    data <- DataFromFile("data/SeasonalModelSims.csv").observations.runWith(Sink.seq)
    vars = Streaming.pilotRun(data.toVector, mod, seasonalParam, resample, particles)
    io <- vars.map { case (n, v) => s"$n, $v" }.
      runWith(Streaming.writeStreamToFile("data/SeasonalPilotRun.csv"))
  } yield io

  res.onComplete(_ => system.terminate())
}

/** Determine the parameters of the seasonal model using PMMH **/
object DetermineSeasonalParameters extends App with SeasonalTestModel with DataProtocols {

  // specify a prior distribution on the parameters
  def prior(p: Parameters) = { p match {
    case LeafParameter(Some(v), BrownianParameter(m, c, mu, sigma)) =>
      Gamma(0.5, 1.0).logPdf(v) +
      m.mapValues(Gaussian(0.5, 3.0).logPdf(_)).reduce(_+_) + 
      c.mapValues(Gamma(0.05, 2.0).logPdf(_)).reduce(_+_) + 
      mu.mapValues(Gaussian(0.1, 3.0).logPdf(_)).reduce(_+_) + 
      sigma.mapValues(Gamma(0.2, 3.0).logPdf(_)).reduce(_+_)
  }}

  // specify the resampling scheme
  def resample: Resample[State, Id] = Resampling.treeStratifiedResampling _

  // the covariance and scale of the proposal distribution
  // the proposal distribution is Multivariate Normal, MVN(0, scale^2 sigma)
  val sigma = DenseMatrix.eye[Double](9)
  val scale = 0.05

  // Specify the data to use in a batch 
  def iters(chain: Int): Future[IOResult] = for {
    data <- DataFromFile("data/SeasonalModelSims.csv").observations.take(400).runWith(Sink.seq)
    filter = (p: Parameters) => ParticleFilter.likelihood(data.toVector, resample, 300)(mod(p))
    pmmh = ParticleMetropolisSerial(filter, seasonalParam, Parameters.perturbMvn(scale, sigma), prior)
    io <- pmmh.
      params.
      take(10000).
      via(Streaming.monitorStream).
      map(_.show).
      runWith(Streaming.writeStreamToFile(s"data/SeasonalModelParams-$chain.csv"))
  } yield io

  Future.sequence((1 to 2).map(iters)).
    onComplete(_ => system.terminate())
}
