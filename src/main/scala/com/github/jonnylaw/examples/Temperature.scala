package com.github.jonnylaw.examples

import com.github.jonnylaw.model.POMP._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import java.nio.file.{Path, Paths}
import akka.util.ByteString

import com.github.jonnylaw.model._
import com.github.jonnylaw.model.Streaming._
import com.github.jonnylaw.model.DataTypes._
import com.github.jonnylaw.model.SimData._
import com.github.jonnylaw.model.Parameters._
import com.github.jonnylaw.model.StateSpace._
import java.io.PrintWriter
import breeze.linalg.{DenseVector, diag}
import cats.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

trait TemperatureModel {
  val data: Vector[Data] = scala.io.Source.fromFile("TrainingTemperature.csv").getLines.toVector.
      drop(1).
      map(a => a.split(",")).
      map(rs => Data(rs(5).toDouble, rs(4).toDouble, None, None, None))

  // define the model
  val linearParam: Parameters = LeafParameter(
    GaussianParameter(14.987419437458, 0.123760233465146),
    Some(0.0662326780360091),
    BrownianParameter(-0.00792688278955239, 0.678847257028525))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector(-0.675768430356229, -0.535485175700474, -0.839863354676339, -0.585465398276856, -0.577353725232412, -0.597149665648091),
      diag(DenseVector(0.101688171314269, 0.151715314425034, 0.111050425853816, 0.0472460213102321, 0.347601923518614, 0.0659490397918582))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-0.817854451851972, -1.01286835908201, -0.960259509475283, -0.222493704420246, -0.140279342626173, -0.406591406745326),
      alpha = DenseVector(0.0662760564571053, 0.173348945772469, 0.0734761800385968, 0.144486171592152, 0.0552416958030416, 0.102607781197052),
      sigma = DenseVector(0.228185104119382, 0.33366219666742, 0.139685314234609, 0.176597498964931, 0.245320195111654, 0.272681259330769)))
  val seasonalParamWeekly = LeafParameter(
    GaussianParameter(DenseVector(-0.675768430356229, -0.535485175700474, -0.839863354676339, -0.585465398276856, -0.577353725232412, -0.597149665648091),
      diag(DenseVector(0.101688171314269, 0.151715314425034, 0.111050425853816, 0.0472460213102321, 0.347601923518614, 0.0659490397918582))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-0.817854451851972, -1.01286835908201, -0.960259509475283, -0.222493704420246, -0.140279342626173, -0.406591406745326),
      alpha = DenseVector(0.0662760564571053, 0.173348945772469, 0.0734761800385968, 0.144486171592152, 0.0552416958030416, 0.102607781197052),
      sigma = DenseVector(0.228185104119382, 0.33366219666742, 0.139685314234609, 0.176597498964931, 0.245320195111654, 0.272681259330769)))

  val weeklyParams = linearParam |+| seasonalParamDaily |+| seasonalParamWeekly
  val dailyParams = linearParam |+| seasonalParamDaily

  val poisson = LinearModel(stepBrownian)
  val daily = SeasonalModel(24, 3, stepOrnstein)
  val weekly = SeasonalModel(24*7, 3, stepOrnstein)
  val weeklyMod = poisson |+| daily |+| weekly
  val dailyMod = poisson |+| daily
}

object SimTemperatureModel extends App {
  val temp = new TemperatureModel {}

  val model = temp.weeklyMod(temp.weeklyParams)

  val sims = simData(temp.data.map(_.t).sorted, model)

  val pw = new PrintWriter("SimTemperature.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

/**
  * Remove duplicate readings where the temperature is exactly the same
  */
object DedupData extends App {
  val data: List[Array[String]] = scala.io.Source.
    fromFile("Temperature.csv").getLines.toList.
    map(a => a.split(","))

  def compress(ls: List[Array[String]]): List[Array[String]] =
    ls.foldRight(List[Array[String]]()) { (h, r) =>
      if (r.isEmpty || r.head(4) != h(4)) h :: r
      else r
    }

  val pw = new PrintWriter("TemperatureNoDuplicates.csv")
  pw.write(compress(data).map(a => a.mkString(", ")).mkString("\n"))
  pw.close()
}

object TemperatureModelGetParameters {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("FitTemperature")
    implicit val materializer = ActorMaterializer()

    val temp = new TemperatureModel {}

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = Filter(temp.dailyMod, ParticleFilter.multinomialResampling)
    val mll = filter.llFilter(temp.data, temp.data.map(_.t).min)(particles) _

    runPmmhToFile(s"Temperature-$delta-$particles", 4,
      temp.dailyParams, mll, Parameters.perturb(delta), iters)
  }
}

object WeeklyTempModelGetParams {
  def main(args: Array[String]) = {
    implicit val system = ActorSystem("FitTemperatureWeekly")
    implicit val materializer = ActorMaterializer()

    val temp = new TemperatureModel {}

    val data: Vector[Data] = scala.io.Source.fromFile("WeeklyTemperature.csv").getLines.toVector.
      drop(1).
      map(a => a.split(",")).
      map(rs => Data(rs(5).toDouble, rs(4).toDouble, None, None, None))

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val filter = Filter(temp.weeklyMod, ParticleFilter.multinomialResampling)
    val mll = filter.llFilter(data, temp.data.map(_.t).min)(particles) _

    runPmmhToFile(s"Temperature-$delta-$particles", 4,
      temp.weeklyParams, mll, Parameters.perturb(delta), iters)
  }
}

object FilterTemperatureModel extends App {
  val temp = new TemperatureModel {
    override val data = scala.io.Source.fromFile("TestTemperature.csv").getLines.toVector.
      drop(1).
      take(2000).
      map(a => a.split(",")).
      map(rs => Data(rs(5).toDouble, rs(4).toDouble, None, None, None))
  }
  val meanParams: Parameters = temp.dailyParams
  val mod = temp.dailyMod(meanParams)

  val filter = Filter(temp.dailyMod, ParticleFilter.multinomialResampling)

  val filtered = filter.filterWithIntervals(temp.data, temp.data.map(_.t).min)(1000)(meanParams)

  val pw = new PrintWriter("TemperatureFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object ForecastTemperatureModel extends App {
  implicit val system = ActorSystem("ForecastTemperature")
  implicit val materializer = ActorMaterializer()

  val temp = new TemperatureModel {}
  val meanParams: Parameters = temp.dailyParams
  val mod = temp.dailyMod(meanParams)

  val filter = Filter(temp.dailyMod, ParticleFilter.multinomialResampling)

  // calculate last filtered state using the parameters
  val filtered = filter.accFilter(temp.data, temp.data.map(_.t).min)(1000)(meanParams)
  val lastState = ParticleFilter.multinomialResampling(filtered.last.particles, filtered.last.weights)

  val times: Vector[Time] = scala.io.Source.fromFile("TestTemperature.csv").getLines.toVector.
      drop(1).
      take(2000).
      map(a => a.split(",")).
      map(rs => rs(5).toDouble).sorted

  Source(times).via(forecastFlow(lastState, times.head, mod)).
    map(f => ByteString(s"$f\n") ).
    runWith(FileIO.toPath(Paths.get(s"ForecastTemperature.csv"))).
    onComplete(_ => system.terminate)
}

object OneStepForecastTemperature extends App {
  implicit val system = ActorSystem("ForecastTemperature")
  implicit val materializer = ActorMaterializer()

  val temp = new TemperatureModel {}

  val data = scala.io.Source.fromFile("TestTemperature.csv").getLines.toVector.
    drop(1).
    take(2000).
    map(a => a.split(",")).
    map(rs => Data(rs(5).toDouble, rs(4).toDouble, None, None, None)).
    sortBy(_.t)

  val filter = Filter(temp.dailyMod, ParticleFilter.multinomialResampling)
  val forecast = filter.getOneStepForecast(data.map(_.t).min)(5000)(temp.dailyParams)

  Source(data).via(forecast).
    drop(1).
    map(f => ByteString(s"$f\n") ).
    runWith(FileIO.toPath(Paths.get(s"oneStepForecastTemperature.csv"))).
    onComplete(_ => system.terminate)
}
