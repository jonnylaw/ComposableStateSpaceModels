
package com.github.jonnylaw.examples.urbanobservatory

import akka.stream.scaladsl._
import akka.stream._
import akka.util.ByteString
import java.nio.file.Paths
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.http.scaladsl.server.Directives._
import breeze.stats.distributions._
import breeze.numerics.{log, exp}
import breeze.linalg.{diag, DenseVector}
import cats._
import cats.implicits._
import com.github.jonnylaw.model._
import TrafficDatabaseTables._
import DataProtocols._
import com.github.nscala_time.time.Imports._
import com.typesafe.config._
import slick.jdbc.SQLiteProfile.api._
import spray.json._

trait NegBinTrafficModel {
  def dailyIndep = Model.seasonalModel(24, 4, Sde.ouProcess(8))
  def weeklyIndep = Model.seasonalModel(24*7, 2, Sde.ouProcess(4))
  def model = Model.negativeBinomial(Sde.brownianMotion(1)) |+| dailyIndep |+| weeklyIndep

  val ziParams: Rand[com.github.jonnylaw.model.Parameters] = {
    for {
      s <- Uniform(2.0, 6.0)
      m0 <- Uniform(0.0, 0.5)
      c0 <- Uniform(0.01, 0.5)
      sigma <- Uniform(0.01, 0.02)
    } yield com.github.jonnylaw.model.Parameters.leafParameter(
      Some(log(s)),
      SdeParameter.brownianParameter(m0, log(c0), log(sigma)))
  }

  val dailyParams: Rand[com.github.jonnylaw.model.Parameters] = for {
      m0 <- Uniform(-2.0, 0.0)
      c0 <- Uniform(0.5, 1.5)
      t <- Applicative[Rand].replicateA(8, Uniform(-0.2, 0.2))
      a <- Uniform(0.0, 0.2)
      s <- Uniform(0.1, 0.5)
    } yield com.github.jonnylaw.model.Parameters.leafParameter(None, SdeParameter.ouParameter(m0, log(c0), log(a), log(s))(t: _*))

  val weeklyParams: Rand[com.github.jonnylaw.model.Parameters] = for {
      m0 <- Uniform(-2.0, 0.0)
      c0 <- Uniform(0.5, 1.5)
      t <- Applicative[Rand].replicateA(4, Uniform(-0.2, 0.2))
      a <- Uniform(0.0, 0.2)
      s <- Uniform(0.1, 0.5)
  } yield com.github.jonnylaw.model.Parameters.leafParameter(None, SdeParameter.ouParameter(m0, log(c0), log(a), log(s))(t: _*))

  val params = for {
    p1 <- ziParams
    p2 <- dailyParams
    p3 <- weeklyParams
  } yield p1 |+| p2 |+| p3

  val negBinPriorDrift = (p: com.github.jonnylaw.model.Parameters) => (p: @unchecked) match {
    case LeafParameter(Some(s), BrownianParameter(m0, c0, sigma)) =>
      Gamma(1.0, 3.0).logPdf(exp(s)) + 
      Gaussian(0.5, 2.5).logPdf(m0) +
      Gamma(2.0, 2.5).logPdf(exp(c0)) +
      Gamma(0.01, 10.0).logPdf(exp(sigma))
    case _ => throw new Exception
  }

  def priorDaily(p: com.github.jonnylaw.model.Parameters) = (p: @unchecked) match {
    case LeafParameter(_, OuParameter(m0, c0, alpha, sigma, theta)) =>
      Gaussian(-0.5, 3.0).logPdf(m0) +
      Gamma(1.0, 10.0).logPdf(exp(c0)) +
      theta.map(Gaussian(-1.0, 3.0).logPdf(_)).sum +
      Gaussian(0.1, 3.0).logPdf(exp(alpha)) +
      Gamma(1.0, 3.0).logPdf(exp(sigma))
    case _ => throw new Exception
  }

  def priorWeekly(p: com.github.jonnylaw.model.Parameters) = (p: @unchecked) match {
    case LeafParameter(_, OuParameter(m0, c0, alpha, sigma, theta)) =>
      Gaussian(-0.5, 3.0).logPdf(m0) +
      Gamma(3.0, 3.0).logPdf(exp(c0)) +
      theta.map(Gaussian(-1.0, 3.0).logPdf(_)).sum +
      Gaussian(0.1, 3.0).logPdf(exp(alpha)) +
      Gamma(2, 2.5).logPdf(exp(sigma))
    case _ => throw new Exception
  }

  def negBinPrior = (p: com.github.jonnylaw.model.Parameters) => (p: @unchecked) match {
    case BranchParameter(BranchParameter(drift, daily), weekly) =>
      negBinPriorDrift(drift) + priorDaily(daily) + priorWeekly(weekly)
    case _ => throw new Exception
  }

}

/**
  * Perform parameter inference by reading past information from the database
  */
object PMMHFromDatabase extends App with NegBinTrafficModel {
  implicit val system = ActorSystem("Server")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  // specify the database to use, loading information using typesafe config
  val db = Database.forConfig("sqlite1")
  val prop = diag(DenseVector.fill(23)(0.01))

  val trafficQuery = trafficFlow.map(t => (t.datetime, t.reading))

  val dataStream = db.stream(trafficQuery.result)

  def resample: Resample[State] = Resampling.systematicResampling _
  val particles = 500
 
  Source.fromPublisher(dataStream).
    map { case (t, r) => TimestampObservation(t, t.getMillis(), Some(r)) }.
    grouped(586).
    mapConcat(d => (1 to 2).map(chain => (d, chain))).
    mapAsync(2) { case (data, chain) =>
      val filter = ParticleFilter.filterLlState(data.toVector, resample, particles) compose model
      val pmmh = MetropolisHastings.pmmhState(params.draw,
        com.github.jonnylaw.model.Parameters.perturbMvn(prop), (from, to) => 0.0, negBinPrior)

      pmmh(filter).
        map(_.toJson.compactPrint).
        runWith(Streaming.writeStreamToFile(s"data/TrafficFlow/TrafficFlowPosterior-$chain.json"))
    }.
    runWith(Sink.onComplete{ s =>
      println(s)
      system.terminate
    })
}
