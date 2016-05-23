import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import scala.concurrent.{duration, Await}
import scala.concurrent.duration._
import akka.util.ByteString

import model._
import model.Model._
import model.Filtering._
import model.POMP.{PoissonModel, SeasonalModel, LinearModel, LogGaussianCox}
import model.DataTypes._
import model.{State, Model}
import model.SimData._
import model.Utilities._
import model.State._
import model.Parameters._
import model.StateSpace._
import java.io.{PrintWriter, File}
import breeze.stats.distributions.Gaussian
import breeze.linalg.{DenseVector, diag}
import breeze.stats.variance

object SimulateLGCP extends App {
  /** Define the model **/
  val params = LeafParameter(
    GaussianParameter(0.0, 1.0),
    None,
    BrownianParameter(0.0, 1.0))

  val mod = LogGaussianCox(stepBrownian)

  val sims = simLGCP(0.0, 3.0, mod(params), 2)

  val pw = new PrintWriter("lgcpsims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object SimulateSeasonalPoisson extends App {
  val poissonParams = LeafParameter(
    GaussianParameter(0.0, 1.0),
    None,
    BrownianParameter(0.0, 1.0))
  val seasonalParams = LeafParameter(
    GaussianParameter(DenseVector(Array.fill(6)(0.0)),
      diag(DenseVector(Array.fill(6)(1.0)))),
    None,
    BrownianParameter(Vector.fill(6)(0.0), Vector.fill(6)(1.0)))

  val params = poissonParams |+| seasonalParams
  val mod = Model.op(PoissonModel(stepBrownian), SeasonalModel(24, 3, stepBrownian))

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(params))

  val pw = new PrintWriter("seasonalPoissonSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object SimulateOrnstein {
  def main(args: Array[String]) = {
    val p = OrnsteinParameter(theta = 6.0, alpha = 0.01, sigma = 0.3)

    val sims = simSdeStream(LeafState(Vector(Gaussian(6.0, 1.0).draw)), 0.0, 300.0, 1, stepOrnstein(p)).toVector

    val pw = new PrintWriter("OrnsteinSims.csv")
    pw.write(sims.mkString("\n"))
    pw.close()
  }
}
