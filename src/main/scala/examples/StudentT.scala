package examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import scala.concurrent.{duration, Await}
import scala.concurrent.duration._
import akka.util.ByteString

import model._
import model.UnparamModel._
import model.Streaming._
import model.POMP.{PoissonModel, SeasonalModel, LinearModel, BernoulliModel, studentTModel}
import model.DataTypes._
import model.{State, Model}
import model.SimData._
import model.Utilities._
import model.State._
import model.Parameters._
import model.StateSpace._
import java.io.{PrintWriter, File}
import breeze.linalg.{DenseVector, diag}
import cats.implicits._

object SeasStudentT extends App {
  val tparams: Parameters = LeafParameter(
    GaussianParameter(0.0, 3.0),
    Some(0.3),
    OrnsteinParameter(3.0, 1.0, 0.5))
  val seasParams: Parameters = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.0), diag(DenseVector.fill(6)(3.0))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(2.0), DenseVector.fill(6)(0.5), DenseVector.fill(6)(0.3)))

  val p = tparams |+| seasParams

  val unparamMod = studentTModel(stepOrnstein, 5) |+| SeasonalModel(24, 3, stepOrnstein)
  val mod = unparamMod(p)

  val times = (1 to 7*24).map(_.toDouble).toList
  val sims = simData(times, mod)

  val pw = new PrintWriter("seastdistSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object GetSeasTParams extends App {
  val tparams: Parameters = LeafParameter(
    GaussianParameter(0.0, 3.0),
    Some(0.3),
    OrnsteinParameter(3.0, 1.0, 0.5))
  val seasParams: Parameters = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.0), diag(DenseVector.fill(6)(3.0))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(2.0), DenseVector.fill(6)(0.5), DenseVector.fill(6)(0.3)))

  val p = tparams |+| seasParams

  val unparamMod = studentTModel(stepOrnstein, 5) |+| SeasonalModel(24, 3, stepOrnstein)

  /**
    * Parse the timestamp and observations from the simulated data
    */
  val data = scala.io.Source.fromFile("seastdistSims.csv").getLines.
    map(a => a.split(",")).
    map(rs => rs map (_.toDouble)).
    map(rs => Data(rs.head, rs(1), None, None, None))

  val mll = Filter(unparamMod, ParticleFilter.multinomialResampling, 0.0).llFilter(data.toVector.sortBy(_.t)) _

  runPmmhToFile("seasTmcmc", 4, p, mll, Parameters.perturb(0.1), 200, 10000)
}
