import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import scala.concurrent.{duration, Await}
import scala.concurrent.duration._
import akka.util.ByteString

import model._
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
import cats.implicits._

/**
  * A model to use for the examples in this class
  */
trait BernoulliModel {
  val p = LeafParameter(
    GaussianParameter(6.0, 1.0),
    None,
    BrownianParameter(mu = 0.1, sigma = 1.0))
  
  val model = BernoulliModel(stepBrownian)
}

/**
  * Simulate 100 observaitions from a simple bernoulli model
  */
object SimulateBernoulli extends App {

  val mod = new BernoulliModel {}
  
  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod.model(mod.p))

  val pw = new PrintWriter("BernoulliSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

/**
  * Filter observations as a batch, return the state and credible intervals
  */
object FilterBernoulli extends App {
  // read in the data from a csv file and parse it to a Data object
  // without the state, eta and gamma
  val data = scala.io.Source.fromFile("BernoulliSims.csv").getLines.
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
    toVector

  val mod = new BernoulliModel {}
  
  val filtered = Filter(mod.model, ParticleFilter.multinomialResampling, 0.0).accFilter(data)(1000)(mod.p)

  val pw = new PrintWriter("BernoulliFiltered.csv")
  pw.write(filtered.draw.mkString("\n"))
  pw.close()
}

/**
  * An example showing real time filtering of observations arriving as a stream
  */
object FilterBernoulliOnline extends App {
  implicit val system = ActorSystem("FilterBernoulliOnline")
  implicit val materializer = ActorMaterializer()

  val model = new BernoulliModel {}
  val mod = model.model(model.p)
  val observations = simStream(mod, 0, t0 = 0.0)

  // write the observations to a file
  observations.
    take(100).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("OnlineBern.csv")))

  // particles and initial state for particle filter
  val n = 1000
  val t0 = 0.0 // replace with first time point
  val particleCloud = Vector.fill(n)(mod.x0.draw)

  val pf = Filter(model.model, ParticleFilter.multinomialResampling, 0.0)
  
  // Use scan to filter a stream, which allows us to output the estimated state as the observations arrive
  pf.filter(observations)(n)(model.p).
    drop(1). // drop the initial state, with no corresponding observation
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("filteredBernoulliOnline.csv")))

  Thread.sleep(10000) // sleep for 10 seconds

  system.shutdown
}

/**
  * Simulate a brownian motion state space 
  */
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

/**
  * Simulate an Ornstein-Uhlenbeck state space
  */
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

/**
  * Simulate a simple composed model, a bernoulli model with seasonal probability
  */
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
  val mod = BernoulliModel(stepBrownian) |+| SeasonalModel(24, 3, stepBrownian)

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(params))

  val pw = new PrintWriter("seasonalBernoulliSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}
