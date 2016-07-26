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
import model.Streaming._
import model.ParticleFilter._
import model.POMP.{PoissonModel, SeasonalModel, LinearModel, LogGaussianCox, BernoulliModel}
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

trait LgcpModel {
  /** Define the model **/
  val params = LeafParameter(
    GaussianParameter(0.0, 1.0),
    None,
    OrnsteinParameter(3.0, 0.5, 1.0))

  val model = LogGaussianCox(stepOrnstein)
}

object SimulateLGCP extends App {
  val mod = new LgcpModel {}

  val sims = simLGCP(0.0, 10.0, mod.model(mod.params), 2)

  val pw = new PrintWriter("lgcpsims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object FilteringLgcp extends App {
  val data = scala.io.Source.fromFile("lgcpsims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs.head.toDouble, rs(1).toDouble, None, None, None)).
    filter(d => d.observation == 1).
    toVector

  val mod = new LgcpModel {}

  val filtered = FilterLgcp(mod.model, ParticleFilter.multinomialResampling, 2, data.map(_.t).min).accFilter(data.sortBy(_.t))(1000)(mod.params)

  val pw = new PrintWriter("LgcpFiltered.csv")
  pw.write(filtered.draw.mkString("\n"))
  pw.close()
}

object GetLgcpParams {
  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    implicit val system = ActorSystem("GetLgcpParams")
    implicit val materializer = ActorMaterializer()

    val data = scala.io.Source.fromFile("lgcpsims.csv").getLines.
      map(a => a.split(",")).
      map(rs => Data(rs.head.toDouble, rs(1).toDouble, None, None, None)).
      toVector

    val mod = new LgcpModel {}

    val mll = FilterLgcp(mod.model, ParticleFilter.multinomialResampling, 2, data.map(_.t).min).llFilter(data.sortBy(_.t))(200) _

    val iterations = 10000

    // the PMMH algorithm is defined as an Akka stream,
    // this means we can write the iterations to a file as they are generated
    // therefore we use constant time memory even for large MCMC runs
    val delta = args.map(_.toDouble).toVector
    val iters = ParticleMetropolisRand(mll, mod.params, Parameters.perturbIndep(delta)).iters

    iters.
      via(monitorStream(1000, 1)).
      runWith(Sink.ignore)

    iters.
      map(s => s.params).
      take(iterations).
      map( p => ByteString(s"$p\n")).
      runWith(FileIO.toFile(new File("LgcpMCMC.csv")))
  }
}
