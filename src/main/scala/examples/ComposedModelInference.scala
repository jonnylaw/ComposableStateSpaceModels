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
import model.POMP.{PoissonModel, SeasonalModel, LinearModel, BernoulliModel, studentTModel}
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
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import breeze.numerics.exp
import cats.implicits._

/**
  * Define a model to use throughout the examples in this file
  */
trait TestModel {
  val poissonParams: Parameters = LeafParameter(
    GaussianParameter(3.0, 0.5),
    None,
    BrownianParameter(0.01, 0.3))
  val seasonalParams: Parameters = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.0),
      diag(DenseVector.fill(6)(0.5))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(1.0), DenseVector.fill(6)(0.1), DenseVector.fill(6)(0.1)))

  val params = poissonParams |+| seasonalParams
  val model = PoissonModel(stepBrownian) |+| SeasonalModel(24, 3, stepOrnstein)
}

/**
  * Simulate a poisson model, with seasonal rate parameter
  */
object SimulateSeasonalPoisson extends App {
  val mod = new TestModel {}

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod.model(mod.params))

  val pw = new PrintWriter("seasonalPoissonSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

/**
  * Filter the simulated seasonal poisson data in a batch
  */
object FilteringSeasonalPoisson extends App {
  val mod = new TestModel {}

  // read the data from file and parse it into a Data class 
  val data = scala.io.Source.fromFile("seasonalPoissonSims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs(0).toDouble, rs(1).toDouble, None, None, None)).
    toVector

  // Define the particle filter
  val filter = Filter(mod.model, ParticleFilter.multinomialResampling, data.map(_.t).min)

  // Run the particle filter over the observed data using 1,000 particles
  val filtered = filter.filterWithIntervals(data)(1000)(mod.params)

  val pw = new PrintWriter("seasonalPoissonFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

object DetermineComposedParams extends App {
  implicit val system = ActorSystem("DeterminePoissonParams")
  implicit val materializer = ActorMaterializer()

  val mod = new TestModel {}

  val data = scala.io.Source.fromFile("seasonalPoissonSims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs(0).toDouble, rs(1).toDouble, None, None, None)).
    toVector

  val mll = Filter(mod.model, ParticleFilter.multinomialResampling, data.map(_.t).min).llFilter(data)(200) _

  val iters = ParticleMetropolis(mll, mod.params, Parameters.perturb(0.05)).iters

  val pw = new PrintWriter("SeasonalPoissonParamsLol.csv")
  pw.write(iters.sample(10000).mkString("\n"))
  pw.close()
}

object FilterOnline extends App {
  implicit val system = ActorSystem("StreamingPoisson")
  implicit val materializer = ActorMaterializer()

  // define the model
  val poissonParams: Parameters = LeafParameter(
    GaussianParameter(0.3, 0.5),
    None,
    BrownianParameter(0.1, 1.0))
  val seasonalParams: Parameters = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.3),
      diag(DenseVector.fill(6)(0.5))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(0.3), DenseVector.fill(6)(0.5), DenseVector.fill(6)(0.1)))

  val params = poissonParams |+| seasonalParams
  val unparamMod = PoissonModel(stepBrownian) |+| SeasonalModel(24, 3, stepOrnstein)
  val mod = unparamMod(params)

  // we can simulate from the process as a stream
  val initialObservation = simStep(mod.x0.draw, 0.0, 1.0, mod)
  val dt = 0.1 // specify the time step of the process

  // unfold holds onto the first item in the Some Tuple and uses it in the next application of the anonymous function
  val observations = Source.unfold(initialObservation){d =>
    Some((simStep(d.sdeState.get, d.t + dt, dt, mod), d))
  }.
    take(100)

  // write the observations to a file
  observations.
    map(a => ByteString(s"$a\m")).
    runWith(FileIO.toFile(new File("OnlineComposedModel.csv")))

  // particles and initial state for particle filter
  val n = 1000
  val mll = Filter(unparamMod, ParticleFilter.multinomialResampling, 0.0)

  mll.filter(observations)(n)(params).
    drop(1).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("OnlineComposedModelFiltered.csv")))

  Thread.sleep(25000) // sleep for 25 seconds

  system.shutdown
}
