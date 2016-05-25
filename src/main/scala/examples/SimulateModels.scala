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
import model.POMP.{PoissonModel, SeasonalModel, LinearModel, BernoulliModel}
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

object SimulateBernoulli extends App {
  val p = LeafParameter(
    GaussianParameter(6.0, 1.0),
    None,
    OrnsteinParameter(theta = 6.0, alpha = 0.05, sigma = 1.0))
  val mod = BernoulliModel(stepOrnstein)

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(p))

  val pw = new PrintWriter("BernoulliSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object SeasonalBernoulli extends App {
  val bernoulliParams = LeafParameter(
    GaussianParameter(0.0, 1.0),
    None,
    BrownianParameter(0.1, 0.3))
  val seasonalParams = LeafParameter(
    GaussianParameter(DenseVector(Array.fill(6)(0.0)),
      diag(DenseVector(Array.fill(6)(1.0)))),
    None,
    BrownianParameter(DenseVector(Array.fill(6)(0.1)), diag(DenseVector(Array.fill(6)(0.4)))))

  val params = bernoulliParams |+| seasonalParams
  val mod = Model.op(BernoulliModel(stepBrownian), SeasonalModel(24, 3, stepBrownian))

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(params))

  val pw = new PrintWriter("seasonalBernoulliSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object FilterBernoulli extends App {
  // read in the data from a csv file and parse it to a Data object
  // without the state, eta and gamma
  val data = scala.io.Source.fromFile("BernoulliSims.csv").getLines.
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
    toVector

  val p = LeafParameter(
    GaussianParameter(6.0, 1.0),
    None,
    OrnsteinParameter(theta = 6.0, alpha = 0.05, sigma = 1.0))
  
  val mod = BernoulliModel(stepOrnstein)

  val filtered = bootstrapPf(1000, data, mod)(p)

  val pw = new PrintWriter("BernoulliFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object DetermineBernoulliParameters extends App {
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val system = ActorSystem("SimBernoulliParameters")
  implicit val materializer = ActorMaterializer()

  // read in the data from a csv file and parse it to a Data object
  // without the state, eta and gamma
  val data = scala.io.Source.fromFile("BernoulliSims.csv").getLines.
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
    toVector

  // specify the parameters and model we used to create the data
  // in the real world, we would not have a good starting point for the parameters
  // so the MCMC algorithm wouldn't immediately start efficiently exploring the posterior
  // iterations are removed from the MCMC output to counteract this, in a period called burnin
  val p = LeafParameter(
    GaussianParameter(6.0, 1.0),
    None,
    OrnsteinParameter(theta = 6.0, alpha = 0.05, sigma = 1.0))
  
  val mod = BernoulliModel(stepOrnstein)

  // the marginal log-likelihood
  val mll = pfMllFold(data, mod)(200)

  val iterations = 10000

  // the PMMH algorithm is defined as an Akka stream,
  // this means we can write the iterations to a file as they are generated
  // therefore we use constant time memory even for large MCMC runs
  val iters = ParticleMetropolis(mll, 0.5).iters(p)

  iters.
    via(monitorStream(1000, 1)).
    runWith(Sink.ignore)

  iters.
    map(s => s.params).
    take(iterations).
    map( p => ByteString(s"$p\n")).
    runWith(FileIO.toFile(new File("BernoulliMCMC.csv")))
}

object SimulateSeasonalPoisson extends App {
  val poissonParams = LeafParameter(
    GaussianParameter(0.3, 0.5),
    None,
    OrnsteinParameter(0.3, 0.5, 0.1))
  val seasonalParams = LeafParameter(
    GaussianParameter(DenseVector(Array.fill(6)(0.3)),
      diag(DenseVector(Array.fill(6)(0.5)))),
    None,
    OrnsteinParameter(Vector.fill(6)(0.3), Vector.fill(6)(0.5), Vector.fill(6)(0.1)))

  val params = poissonParams |+| seasonalParams
  val mod = Model.op(PoissonModel(stepOrnstein), SeasonalModel(24, 3, stepOrnstein))

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(params))

  val pw = new PrintWriter("seasonalPoissonSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object SimulateOrnstein {
  def main(args: Array[String]) = {
    val p = OrnsteinParameter(theta = 6.0, alpha = 0.05, sigma = 1.0)
    val initialState = LeafState(DenseVector(Gaussian(6.0, 1.0).draw))

    val sims = simSdeStream(initialState, 0.0, 300.0, 1, stepOrnstein(p)).toVector

    val pw = new PrintWriter("OrnsteinSims.csv")
    pw.write(sims.mkString("\n"))
    pw.close()
  }
}
