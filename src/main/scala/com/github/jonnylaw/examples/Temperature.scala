package com.github.jonnylaw.examples

import com.github.jonnylaw.model.POMP._

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

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
  val data = scala.io.Source.fromFile("TemperatureFormatted.csv").getLines.toVector.
    drop(1).
    map(a => a.split(",")).
    map(rs => Data(rs.head.toDouble, rs(1).toDouble, None, None, None))

  // define the model
  val linearParam: Parameters = LeafParameter(
    GaussianParameter(15.0, 0.1),
    Some(0.1),
    BrownianParameter(0.01, 0.1))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(-0.5), diag(DenseVector.fill(6)(0.1))),
    None,
    OrnsteinParameter(
      theta = DenseVector.fill(6)(-1.0),
      alpha = DenseVector.fill(6)(0.1),
      sigma = DenseVector.fill(6)(0.2)))

  val initParams = linearParam |+| seasonalParamDaily

  val poisson = LinearModel(stepBrownian)
  val daily = SeasonalModel(24, 3, stepOrnstein)
  val unparamMod = poisson |+| daily
}

object SimTemperatureModel extends App {
  val temp = new TemperatureModel {}
  val model = temp.unparamMod(temp.initParams)

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

    val filter = Filter(temp.unparamMod, ParticleFilter.multinomialResampling)
    val mll = filter.llFilter(temp.data, temp.data.map(_.t).min)(particles) _

    runPmmhToFile(s"Temperature-$delta-$particles", 4,
      temp.initParams, mll, Parameters.perturb(delta), iters)
  }
}

object FilterTemperatureModel extends App {
  val temp = new TemperatureModel {}
  val meanParams: Parameters = ???
  val mod = temp.unparamMod(meanParams)

  val filter = Filter(temp.unparamMod, ParticleFilter.multinomialResampling)

  val filtered = filter.filterWithIntervals(temp.data, temp.data.map(_.t).min)(1000)(meanParams)

  val pw = new PrintWriter("CarsFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object ForecastTemperatureModel extends App {
  val temp = new TemperatureModel {}
  val meanParams: Parameters = ???
  val mod = temp.unparamMod(meanParams)

  val filter = Filter(temp.unparamMod, ParticleFilter.multinomialResampling)

  // calculate last filtered state using the parameters
  val filtered = filter.accFilter(temp.data, temp.data.map(_.t).min)(1000)(meanParams)
  val lastState = ParticleFilter.multinomialResampling(
    filtered.last.particles,
    filtered.last.weights)

  val forecast = forecastData(lastState, temp.data.map(_.t).sorted, mod)

  val pw = new PrintWriter("forecastCars.csv")
  pw.write(forecast.mkString("\n"))
  pw.close()

}
