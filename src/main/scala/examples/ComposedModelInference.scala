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
import model.Model._
import model.Filtering._
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

/**
  * Define a model to use throughout the examples in this file
  */
trait TestModel {
  val poissonParams = LeafParameter(
    GaussianParameter(3.0, 0.5),
    None,
    BrownianParameter(0.01, 0.3))
  val seasonalParams = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.0),
      diag(DenseVector.fill(6)(0.5))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(1.0), DenseVector.fill(6)(0.1), DenseVector.fill(6)(0.1)))

  val params = poissonParams |+| seasonalParams
  val model = Model.op(PoissonModel(stepBrownian), SeasonalModel(24, 3, stepOrnstein))
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

  val data = scala.io.Source.fromFile("seasonalPoissonSims.csv").getLines.
    map(a => a.split(",")).
    map(rs => Data(rs(0).toDouble, rs(1).toDouble, None, None, None)).
    toVector

  val filtered = bootstrapPf(1000, data, mod.model)(mod.params)

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

  val mll = pfMllFold(data, mod.model) _

  runPmmhToFile("poisson", 2, mod.params, mll, Parameters.perturb(0.1), 200, 10000)
}

object FilterOnline extends App {
  implicit val system = ActorSystem("StreamingPoisson")
  implicit val materializer = ActorMaterializer()

  // define the model
    val poissonParams = LeafParameter(
    GaussianParameter(0.3, 0.5),
    None,
    BrownianParameter(0.1, 1.0))
  val seasonalParams = LeafParameter(
    GaussianParameter(DenseVector.fill(6)(0.3),
      diag(DenseVector.fill(6)(0.5))),
    None,
    OrnsteinParameter(DenseVector.fill(6)(0.3), DenseVector.fill(6)(0.5), DenseVector.fill(6)(0.1)))

  val params = poissonParams |+| seasonalParams
  val unparamMod = Model.op(PoissonModel(stepBrownian), SeasonalModel(24, 3, stepOrnstein))
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
  val initState = PfState(0.0, None, Vector.fill(n)(mod.x0.draw), State.zero, IndexedSeq[CredibleInterval]())

  observations.
    scan(initState)((d, y) => filterStepScan(y, d, mod, 200)).
    drop(1).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("OnlineComposedModelFiltered.csv")))

  Thread.sleep(25000) // sleep for 25 seconds

  system.shutdown
}

object Cars {
  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem("FitCars")
    implicit val materializer = ActorMaterializer()

    val data = scala.io.Source.fromFile("cars.csv").getLines.toVector.
      drop(1).
      map(a => a.split(",")).
      map(rs => Data(rs(1).toDouble, rs(2).toDouble, None, None, None))

    // define the model
    val poissonParam = LeafParameter(
      GaussianParameter(6.0, 0.1),
      None,
      OrnsteinParameter(6.0, 0.1, 0.1))
    val seasonalParamDaily = LeafParameter(
      GaussianParameter(DenseVector(-0.5, -0.3, -0.75, -0.3, -0.3, -0.5), diag(DenseVector.fill(6)(0.1))),
      None,
      OrnsteinParameter(
        theta = DenseVector(-1.2, -1.0, -1.0, -0.5, -0.5, -0.7, -0.5, -0.7),
        alpha = DenseVector.fill(6)(0.1),
        sigma = DenseVector.fill(6)(0.2)))

    val initParams = poissonParam |+| seasonalParamDaily

    val poisson = PoissonModel(stepOrnstein)
    val daily = SeasonalModel(24, 3, stepOrnstein)
    val unparamMod = Model.op(poisson, daily)

    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    val mll = pfMll(data.sortBy(_.t), unparamMod) _

    runPmmhToFile("cars", chains = 4, initParams, mll, Parameters.perturb(delta), particles = particles, iterations = iters)
  }
}

object SimCars extends App {
  val poissonParam = LeafParameter(
    GaussianParameter(6.0, 0.1),
    None,
    OrnsteinParameter(6.0, 0.1, 0.1))
  val seasonalParamDaily = LeafParameter(
    GaussianParameter(DenseVector(-0.5, -0.3, -0.75, -0.3, -0.3, -0.5), diag(DenseVector.fill(6)(0.1))),
    None,
    OrnsteinParameter(
      theta = DenseVector(-1.2, -1.0, -1.0, -0.5, -0.5, -0.7, -0.5, -0.7),
      alpha = DenseVector.fill(6)(0.1),
      sigma = DenseVector.fill(6)(0.2)))

  val p = poissonParam |+| seasonalParamDaily

  val poisson = PoissonModel(stepOrnstein)
  val daily = SeasonalModel(24, 3, stepOrnstein)
  val poissonMod = Model.op(poisson, daily)

  val times = (1 to 169).map(_.toDouble).toList
  val sims = simData(times, poissonMod(p))


  val pw = new PrintWriter("simCars.csv")
  pw.write(sims.
    map(data => s"${data.t}, ${data.observation}").
    mkString("\n"))
  pw.close()
}
