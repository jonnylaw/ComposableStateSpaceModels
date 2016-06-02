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
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}
import breeze.linalg.{DenseVector, diag}
import breeze.numerics.exp
import scala.concurrent.ExecutionContext.Implicits.global

object SimulateBernoulli extends App {
  val p = LeafParameter(
    GaussianParameter(6.0, 1.0),
    None,
    BrownianParameter(mu = 0.1, sigma = 1.0))
  
  val mod = BernoulliModel(stepBrownian)
  
  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(p))

  val pw = new PrintWriter("BernoulliSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

object SimulateBrownian extends App {

  implicit val system = ActorSystem("SimBrownian")
  implicit val materializer = ActorMaterializer()

  val p = BrownianParameter(DenseVector(0.1, 0.1), diag(DenseVector(0.1, 0.5)))
  val x0: State = LeafState(MultivariateGaussian(DenseVector(1.0, 1.0), diag(DenseVector(5.0, 5.0))).draw)
  val dt = 0.1

  Source.unfold(x0)(x => Some((stepBrownian(p)(x, dt).draw, x))).
    zip(Source.tick(1 second, 1 second, Unit)).
    map{ case (a, _) => a }.
    runForeach(println)
}

object SeasonalBernoulli extends App {
  val bernoulliParams = LeafParameter(
    GaussianParameter(0.0, 1.0),
    None,
    BrownianParameter(0.1, 1.0))
  val seasonalParams = LeafParameter(
    GaussianParameter(DenseVector(Array.fill(6)(0.0)),
      diag(DenseVector(Array.fill(6)(1.0)))),
    None,
    BrownianParameter(DenseVector.fill(6)(0.1), diag(DenseVector.fill(6)(1.0))))

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
    BrownianParameter(mu = 0.1, sigma = 1.0))
  
  val mod = BernoulliModel(stepBrownian)

  val filtered = bootstrapPf(1000, data, mod)(p)

  val pw = new PrintWriter("BernoulliFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object DetermineBernoulliParameters extends App {

  implicit val system = ActorSystem("DetermineBernoulliParameters")
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
    BrownianParameter(mu = 0.1, sigma = 1.0))
  
  val mod = BernoulliModel(stepBrownian)

  // the marginal log-likelihood
  val mll = pfMll(data, mod)(200)

  val iterations = 10000

  // the PMMH algorithm is defined as an Akka stream,
  // this means we can write the iterations to a file as they are generated
  // therefore we use constant time memory even for large MCMC runs
  val delta = Vector(0.5, 0.05, 0.5, 0.2)
  val iters = ParticleMetropolis(mll, p, Parameters.perturbIndep(delta)).iters

  iters.
    via(monitorStream(1000, 1)).
    runWith(Sink.ignore)

  iters.
    map(s => s.params).
    take(iterations).
    map( p => ByteString(s"$p\n")).
    runWith(FileIO.toFile(new File("BernoulliMCMC.csv")))
}

/**
  * An example showing real time filtering of observations arriving as a stream
  */
object FilterBernoulliOnline extends App {
  implicit val system = ActorSystem("FilterBernoulliOnline")
  implicit val materializer = ActorMaterializer()

  val p = LeafParameter(
    GaussianParameter(6.0, 1.0),
    None,
    OrnsteinParameter(theta = 6.0, alpha = 0.05, sigma = 1.0))
  
  val mod = BernoulliModel(stepOrnstein)(p)
  val observations = simStream(mod, 0, t0 = 0.0)

  // write the observations to a file
  observations.
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("OnlineBern.csv")))

  // particles and initial state for particle filter,
  val n = 1000
  val t0 = 0.0 // replace with first time point
  val particleCloud = Vector.fill(n)(mod.x0.draw)
  val initState = Vector(PfState(t0, None, particleCloud, State.zero, IndexedSeq[CredibleInterval]()))

  // use fold to filter a stream
  // fold will only output once the stream has terminated
  // but we can print, or write to a file inside the fold
  observations.
    fold(initState)((d, y) => filterStep(y, d, mod, 200)).
    map(d => d.reverse).
    mapConcat(identity). // from Stream(Vector(...)) -> Stream(...)
    map(s => PfOut(s.t0, s.observation, s.meanState, s.intervals)).
    drop(1). // drop the initial state, with no corresponding observation
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("filteredBernoulliOnline.csv")))

  Thread.sleep(10000) // sleep for 10 seconds

  system.shutdown
}

object SimulateOrnstein {
  def main(args: Array[String]) = {
    val p = OrnsteinParameter(theta = 1.0, alpha = 0.05, sigma = 1.0)
    val initialState = LeafState(DenseVector(Gaussian(6.0, 1.0).draw))

    val sims = simSdeStream(initialState, 0.0, 300.0, 1, stepOrnstein(p)).toVector

    val pw = new PrintWriter("OrnsteinSims.csv")
    pw.write(sims.mkString("\n"))
    pw.close()
  }
}

object Linear extends App {
  implicit val system = ActorSystem("FilterBernoulliOnline")
  implicit val materializer = ActorMaterializer()

  val p = LeafParameter(GaussianParameter(3.0, 2.0), Some(1.0), BrownianParameter(0.1, 1.0))
  val mod = LinearModel(stepBrownian)

  val times = (0.0 to 50.0 by 0.5).toList
  val sims = simData(times, mod(p))

  val pw = new PrintWriter("LinearSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()

  val n = 500
  val mll = pfMll(sims, mod)(n)

  val iters = ParticleMetropolis(mll, p, Parameters.perturb(0.1)).iters

  iters.
    via(monitorStream(1000, 1)).
    runWith(Sink.ignore)

  iters.
    map(s => s.params).
    take(10000).
    map( p => ByteString(s"$p\n")).
    runWith(FileIO.toFile(new File("LinearMCMC.csv")))
}
