package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import java.nio.file.{Path, Paths}
import akka.util.ByteString
import scala.concurrent.Future
import akka.stream.IOResult

import com.github.jonnylaw.model._
import Streaming._
import DataTypes._
import SimData._
import Parameters._
import StateSpace._
import java.io.PrintWriter
import breeze.linalg.{DenseVector, diag}
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._

trait TrafficModel {
  val data = scala.io.Source.fromFile("poissonCars.csv").getLines.toVector.
    drop(1).
    take(500).
    map(a => a.split(",")).
    map(rs => Data(rs(5).toDouble, rs(2).toDouble, None, None, None))

  // define the model
  val poissonParam: Parameters = LeafParameter(
    GaussianParameter(6.0, 0.1),
    None,
    OrnsteinParameter(6.0, 0.1, 0.1))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(-0.5), diag(DenseVector.fill(6)(0.1))),
    None,
    OrnsteinParameter(
      theta = DenseVector.fill(6)(-1.0),
      alpha = DenseVector.fill(6)(0.1),
      sigma = DenseVector.fill(6)(0.2)))

  val initParams = poissonParam |+| seasonalParamDaily

  // define the model
  val meanPoissonParam: Parameters = LeafParameter(
    GaussianParameter(5.90476244, 0.05428142),
    None,
    OrnsteinParameter(5.91759210, 0.21138829, 0.10249756))
  val meanSeasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector(-0.61342281, -0.80213544, -0.88888523, -0.96405392, -0.08372973, -0.60655922),
      diag(DenseVector(0.07429421, 0.07430095, 0.15234295, 0.05018332, 0.07169831, 0.11063340))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-1.46861526, -0.61782671, -0.40233685, -0.45720772, 0.04869052, -0.17680178),
      alpha = DenseVector(0.20707158, 0.28219862, 0.10223457, 0.06999775, 0.17084591, 0.11341918),
      sigma = DenseVector(0.69927626, 0.59157805, 1.31074547, 0.34254442, 0.18439177, 0.42491019)))

  val meanParams = meanPoissonParam |+| meanSeasonalParamDaily

  val poisson: UnparamModel = PoissonModel(stepOrnstein)
  val daily: UnparamModel = SeasonalModel(24, 3, stepOrnstein)
  val unparamMod = poisson |+| daily
}

object PoissonCars extends App with TrafficModel {
    implicit val system = ActorSystem("FitCars")
    implicit val materializer = ActorMaterializer()

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)
    val mll = filter.llFilter(data, data.map(_.t).min)(particles) _

    runPmmhToFile(s"PoissonTraffic-$delta-$particles", 4,
      initParams, mll, Parameters.perturb(delta), iters)
}

object LgcpCars extends App {
    implicit val system = ActorSystem("FitCarsLgcp")
    implicit val materializer = ActorMaterializer()

    val data = scala.io.Source.fromFile("lgcpCars.csv").getLines.toVector.
      drop(1).
      map(a => a.split(",")).
      map(rs => Data(rs(1).toDouble, true, None, None, None))

    // define the model
    val lgcpParam: Parameters = LeafParameter(
      GaussianParameter(6.0, 0.1),
      None,
      OrnsteinParameter(6.0, 0.1, 0.1))

    val seasonalParamDaily = LeafParameter(
      GaussianParameter(DenseVector.fill(6)(-0.5), diag(DenseVector.fill(6)(0.1))),
      None,
      OrnsteinParameter(
        theta = DenseVector.fill(6)(-1.0),
        alpha = DenseVector.fill(6)(0.1),
        sigma = DenseVector.fill(6)(0.2)))

    val initParams = lgcpParam |+| seasonalParamDaily

    val poisson: UnparamModel = LogGaussianCox(stepOrnstein)
    val daily: UnparamModel = SeasonalModel(24, 3, stepOrnstein)
    val unparamMod = poisson |+| daily

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = FilterLgcp(unparamMod, ParticleFilter.multinomialResampling, 0)
    val mll = filter.llFilter(data, data.map(_.t).min)(particles) _
    val mh = ParticleMetropolis(mll, initParams, Parameters.perturb(delta))

    runPmmhToFile(s"LgcpTraffic-$delta-$particles", 4,
      initParams, mll, Parameters.perturb(delta), iters)
}

/**
  * To determine the mll variance at a given set of parameters, we must perform iterations of the 
  * PMMH algorithm with the proposal distribution being the identity
  * This allows us to determine the optimal number of particles used in the PMMH,
  * at a set of "good" parameters, typically it is said the mll variance should be one
  */
object GetmllVariance extends App with TrafficModel {
    implicit val system = ActorSystem("PilotRun")
    implicit val materializer = ActorMaterializer()

    val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)

    val nParticles = args.head.toInt

    val mll = filter.llFilter(data, data.map(_.t).min)(nParticles) _
    pilotRun(mll, meanParams, 1000).run()
}

object FilterCars extends App with TrafficModel {
  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)

  val filtered = filter.filterWithIntervals(data, data.map(_.t).min)(1000)(meanParams)

  val pw = new PrintWriter("CarsFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object ForecastCars extends App with TrafficModel {
  implicit val system = ActorSystem("ForecastCars")
  implicit val materializer = ActorMaterializer()

  val mod = unparamMod(meanParams)

  val times = (500.0 to 550.0 by 1.0).toList

  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)

  // calculate last filtered state using the parameters
  val filtered = filter.accFilter(data, data.map(_.t).min)(1000)(meanParams)
  val lastState = ParticleFilter.multinomialResampling(filtered.last.particles, filtered.last.weights)

  Source(times).via(forecastFlow(lastState, times.head, mod)).
    map(f => ByteString(s"$f\n") ).
    runWith(FileIO.toPath(Paths.get(s"forecastCars.csv")))

  // val forecast = forecastData(lastState, times, mod)

  // val pw = new PrintWriter("forecastCars.csv")
  // pw.write(forecast.mkString("\n"))
  // pw.close()
}

object PoissonWeekly extends App with TrafficModel {
    val seasonalParamWeekly = LeafParameter(
      GaussianParameter(DenseVector.fill(6)(0.0), diag(DenseVector.fill(6)(0.05))),
      None,
      OrnsteinParameter(
        theta = DenseVector.fill(6)(-0.01),
        alpha = DenseVector.fill(6)(0.3),
        sigma = DenseVector.fill(6)(0.02)))

    override val initParams = meanParams |+| seasonalParamWeekly

    val weekly = SeasonalModel(24*7, 3, stepOrnstein)
    val weeklyMod = unparamMod |+| weekly

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = Filter(weeklyMod, ParticleFilter.multinomialResampling)
    val mll = filter.llFilter(data, data.map(_.t).min)(particles) _
    val mh = ParticleMetropolis(mll, initParams, Parameters.perturb(delta))

    runPmmhToFile(s"PoissonTrafficWeekly-$delta-$particles", 4,
      initParams, mll, Parameters.perturb(delta), iters)
}

object OneStepForecastTraffic extends App with TrafficModel {
  implicit val system = ActorSystem("ForecastCars")
  implicit val materializer = ActorMaterializer()

  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)
  val forecast = filter.getOneStepForecast(499.0)(5000)(meanParams)

  val result: Future[IOResult] = Source(data).via(forecast).
    drop(1).
    map(f => ByteString(s"$f\n") ).
    runWith(FileIO.toPath(Paths.get(s"oneStepForecastCars.csv")))

  result.onComplete(_ => system.terminate)
}
