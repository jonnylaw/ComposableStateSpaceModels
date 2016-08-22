package examples

import model.POMP._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import scala.concurrent.{duration, Await}
import scala.concurrent.duration._
import akka.util.ByteString
import GraphDSL.Implicits._
import akka.stream.ClosedShape
import java.nio.file.{Path, Paths}
import scala.concurrent.Future

import model._
import model.Streaming._
import model.POMP.{PoissonModel, SeasonalModel, LogGaussianCox}
import model.DataTypes._
import model.{State, Model}
import model.SimData._
import model.Utilities._
import model.ParticleFilter._
import model.State._
import model.Parameters._
import model.StateSpace._
import java.io.{PrintWriter, File}
import breeze.stats.distributions.Gaussian
import breeze.linalg.{DenseVector, diag}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import breeze.numerics.exp
import cats.implicits._

object PoissonCars {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("FitCars")
    implicit val materializer = ActorMaterializer()

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

    val poisson = PoissonModel(stepOrnstein)
    val daily = SeasonalModel(24, 3, stepOrnstein)
    val unparamMod = poisson |+| daily

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = Filter(unparamMod, ParticleFilter.multinomialResampling, data.map(_.t).min)
    val mll = filter.llFilter(data)(particles) _
    val mh = ParticleMetropolis(mll, initParams, Parameters.perturb(delta))

    runPmmhToFile(s"PoissonTraffic-$delta-$particles", 4,
      initParams, mll, Parameters.perturb(delta), iters)
  }
}

object LgcpCars {
  def main(args: Array[String]): Unit = {
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

    val poisson = LogGaussianCox(stepOrnstein)
    val daily = SeasonalModel(24, 3, stepOrnstein)
    val unparamMod = poisson |+| daily

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = FilterLgcp(unparamMod, ParticleFilter.multinomialResampling, 0, data.map(_.t).min)
    val mll = filter.llFilter(data)(particles) _
    val mh = ParticleMetropolis(mll, initParams, Parameters.perturb(delta))

    runPmmhToFile(s"LgcpTraffic-$delta-$particles", 4,
      initParams, mll, Parameters.perturb(delta), iters)
  }
}

/**
  * To determine the mll variance at a given set of parameters, we must perform iterations of the 
  * PMMH algorithm with the proposal distribution being the identity
  * This allows us to determine the optimal number of particles used in the PMMH,
  * at a set of "good" parameters, typically it is said the mll variance should be one
  */
object GetmllVariance {
  def main(args: Array[String]) = {
    implicit val system = ActorSystem("PilotRun")
    implicit val materializer = ActorMaterializer()

    val data = scala.io.Source.fromFile("poissonCars.csv").getLines.toVector.
      drop(1).
      take(500).
      map(a => a.split(",")).
      map(rs => Data(rs(5).toDouble, rs(2).toDouble, None, None, None))

    // define the model
    val poissonParam: Parameters = LeafParameter(
      GaussianParameter(5.90476244, 0.05428142),
      None,
      OrnsteinParameter(5.91759210, 0.21138829, 0.10249756))
    val seasonalParamDaily = LeafParameter(
      GaussianParameter(DenseVector(-0.61342281, -0.80213544, -0.88888523, -0.96405392, -0.08372973, -0.60655922),
        diag(DenseVector(0.07429421, 0.07430095, 0.15234295, 0.05018332, 0.07169831, 0.11063340))),
      None,
      OrnsteinParameter(
        theta = DenseVector(-1.46861526, -0.61782671, -0.40233685, -0.45720772, 0.04869052, -0.17680178),
        alpha = DenseVector(0.20707158, 0.28219862, 0.10223457, 0.06999775, 0.17084591, 0.11341918),
        sigma = DenseVector(0.69927626, 0.59157805, 1.31074547, 0.34254442, 0.18439177, 0.42491019)))

    val meanParams = poissonParam |+| seasonalParamDaily

    val poisson = PoissonModel(stepOrnstein)
    val daily = SeasonalModel(24, 3, stepOrnstein)
    val unparamMod = poisson |+| daily

    val filter = Filter(unparamMod, ParticleFilter.multinomialResampling, data.map(_.t).min)

    val nParticles = args.head.toInt

    val mll = filter.llFilter(data)(nParticles) _
    pilotRun(mll, meanParams, 1000).run
  }
}

object FilterCars extends App {
  val data = scala.io.Source.fromFile("poissonCars.csv").getLines.toVector.
    drop(501).
    map(a => a.split(",")).
    map(rs => Data(rs(5).toDouble, rs(2).toDouble, None, None, None))

  // define the model
   val poissonParam: Parameters = LeafParameter(
    GaussianParameter(5.90476244, 0.05428142),
    None,
    OrnsteinParameter(5.91759210, 0.21138829, 0.10249756))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector(-0.61342281, -0.80213544, -0.88888523, -0.96405392, -0.08372973, -0.60655922), diag(DenseVector(0.07429421, 0.07430095, 0.15234295, 0.05018332, 0.07169831, 0.11063340))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-1.46861526, -0.61782671, -0.40233685, -0.45720772, 0.04869052, -0.17680178),
      alpha = DenseVector(0.20707158, 0.28219862, 0.10223457, 0.06999775, 0.17084591, 0.11341918),
      sigma = DenseVector(0.69927626, 0.59157805, 1.31074547, 0.34254442, 0.18439177, 0.42491019)))

  val meanParams = poissonParam |+| seasonalParamDaily

  val poisson = PoissonModel(stepOrnstein)
  val daily = SeasonalModel(24, 3, stepOrnstein)
  val unparamMod = poisson |+| daily

  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling, data.map(_.t).min)

  val filtered = filter.filterWithIntervals(data)(1000)(meanParams)

  val pw = new PrintWriter("CarsFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object ForecastCars extends App {
  // define the model
   val poissonParam: Parameters = LeafParameter(
    GaussianParameter(5.90476244, 0.05428142),
    None,
    OrnsteinParameter(5.91759210, 0.21138829, 0.10249756))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector(-0.61342281, -0.80213544, -0.88888523, -0.96405392, -0.08372973, -0.60655922), diag(DenseVector(0.07429421, 0.07430095, 0.15234295, 0.05018332, 0.07169831, 0.11063340))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-1.46861526, -0.61782671, -0.40233685, -0.45720772, 0.04869052, -0.17680178),
      alpha = DenseVector(0.20707158, 0.28219862, 0.10223457, 0.06999775, 0.17084591, 0.11341918),
      sigma = DenseVector(0.69927626, 0.59157805, 1.31074547, 0.34254442, 0.18439177, 0.42491019)))

  val meanParams = poissonParam |+| seasonalParamDaily

  val poisson = PoissonModel(stepOrnstein)
  val daily = SeasonalModel(24, 3, stepOrnstein)
  val unparamMod = poisson |+| daily
  val mod = unparamMod(meanParams)

  val times = (500.0 to 550.0 by 1.0).toList

  val data = scala.io.Source.fromFile("poissonCars.csv").getLines.toVector.
    drop(1).
    take(500).
    map(a => a.split(",")).
    map(rs => Data(rs(5).toDouble, rs(2).toDouble, None, None, None))

  val filter = Filter(unparamMod, ParticleFilter.multinomialResampling, data.map(_.t).min)

  // calculate last filtered state using the parameters
  val filtered = filter.accFilter(data)(1000)(meanParams)
  val lastState = ParticleFilter.multinomialResampling(
    filtered.last.particles,
    filtered.last.weights)

  val forecast = forecastData(lastState, times, mod)

  val pw = new PrintWriter("forecastCars.csv")
  pw.write(forecast.mkString("\n"))
  pw.close()
}

object PoissonWeekly {
  def main(args: Array[String]): Unit = {
    // maybe a few more data points
    val data = scala.io.Source.fromFile("poissonCars.csv").getLines.toVector.
      drop(1).
      take(500).
      map(a => a.split(",")).
      map(rs => Data(rs(5).toDouble, rs(2).toDouble, None, None, None))

       val poissonParam: Parameters = LeafParameter(
    GaussianParameter(5.90476244, 0.05428142),
    None,
    OrnsteinParameter(5.91759210, 0.21138829, 0.10249756))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector(-0.61342281, -0.80213544, -0.88888523, -0.96405392, -0.08372973, -0.60655922),
      diag(DenseVector(0.07429421, 0.07430095, 0.15234295, 0.05018332, 0.07169831, 0.11063340))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-1.46861526, -0.61782671, -0.40233685, -0.45720772, 0.04869052, -0.17680178),
      alpha = DenseVector(0.20707158, 0.28219862, 0.10223457, 0.06999775, 0.17084591, 0.11341918),
      sigma = DenseVector(0.69927626, 0.59157805, 1.31074547, 0.34254442, 0.18439177, 0.42491019)))
    val seasonalParamWeekly = LeafParameter(
      GaussianParameter(DenseVector.fill(6)(0.0), diag(DenseVector.fill(6)(0.05))),
      None,
      OrnsteinParameter(
        theta = DenseVector.fill(6)(-0.01),
        alpha = DenseVector.fill(6)(0.3),
        sigma = DenseVector.fill(6)(0.02)))

    val initParams = poissonParam |+| seasonalParamDaily |+| seasonalParamWeekly

    val poisson = PoissonModel(stepOrnstein)
    val daily = SeasonalModel(24, 3, stepOrnstein)
    val weekly = SeasonalModel(24*7, 3, stepOrnstein)
    val unparamMod = poisson |+| daily |+| weekly

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = Filter(unparamMod, ParticleFilter.multinomialResampling, data.map(_.t).min)
    val mll = filter.llFilter(data)(particles) _
    val mh = ParticleMetropolis(mll, initParams, Parameters.perturb(delta))

    runPmmhToFile(s"PoissonTrafficWeekly-$delta-$particles", 4,
      initParams, mll, Parameters.perturb(delta), iters)
  }
}
