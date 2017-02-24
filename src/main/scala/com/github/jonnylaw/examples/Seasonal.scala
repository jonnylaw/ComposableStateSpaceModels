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
    take(300).
    map((d: Data) => d.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/SeasonalModelSims.csv"))).
    onComplete(_ => system.terminate())
}

object FilterSeasonal extends App with SeasonalTestModel {
  val data = FileIO.fromPath(Paths.get("data/SeasonalModelSims.csv")).
    via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    take(300)

  val t0 = 0.0

  val filter = ParticleFilter.filter[ParVector](ParticleFilter.parMultinomialResampling, t0, 1000)

  data.
    via(filter(mod(seasonalParam))).
    map(ParticleFilter.getIntervals(mod(seasonalParam))).
    drop(1).
    map(_.show).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("data/SeasonalModelFiltered.csv"))).
    onComplete(_ => system.terminate())
}
