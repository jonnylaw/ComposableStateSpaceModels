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

object SimulateLGCP extends App {
  /** Define the model **/
  val params = LeafParameter(
    GaussianParameter(1.0, 1.0),
    None,
    OrnsteinParameter(1.0, 0.1, 0.4))

  val mod = LogGaussianCox(stepOrnstein)

  val sims = simLGCP(0.0, 3.0, mod(params), 2)

  val pw = new PrintWriter("lgcpsims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object FilterLgcp extends App {
  val data = scala.io.Source.fromFile("lgcpsims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs.head.toDouble, rs(1).toDouble, None, None, None)).
    filter(d => d.observation == 1).
    toVector

  /** Define the model **/
  val params = LeafParameter(
    GaussianParameter(1.0, 1.0),
    None,
    OrnsteinParameter(1.0, 0.1, 0.4))

  val mod = LogGaussianCox(stepOrnstein)

  val filtered = pfLGCP(1000, data.sortBy(_.t), mod, 2)(params)

  val pw = new PrintWriter("LgcpFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object GetLgcpParams extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val system = ActorSystem("SimBernoulliParameters")
  implicit val materializer = ActorMaterializer()

  val data = scala.io.Source.fromFile("lgcpsims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs.head.toDouble, rs(1).toDouble, None, None, None)).
    toVector

  val params = LeafParameter(
    GaussianParameter(0.0, 1.0),
    None,
    BrownianParameter(0.0, 1.0))

  val mod = LogGaussianCox(stepBrownian)

  val mll = pfLGCPmll(data.sortBy(_.t), mod, 2)(200)

  val iterations = 10000

    // the PMMH algorithm is defined as an Akka stream,
  // this means we can write the iterations to a file as they are generated
  // therefore we use constant time memory even for large MCMC runs
  val iters = ParticleMetropolis(mll, gaussianPerturb(0.1, 0.05)).iters(params)

  iters.
    via(monitorStream(1000, 1)).
    runWith(Sink.ignore)

  iters.
    map(s => s.params).
    take(iterations).
    map( p => ByteString(s"$p\n")).
    runWith(FileIO.toFile(new File("LgcpMCMC.csv")))
}
