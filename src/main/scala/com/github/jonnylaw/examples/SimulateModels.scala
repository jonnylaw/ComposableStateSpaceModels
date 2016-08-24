package com.github.jonnylaw.examples

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.{File, PrintWriter}
import akka.stream.scaladsl._
import scala.concurrent.{duration, Await}
import scala.concurrent.duration._
import akka.util.ByteString
import GraphDSL.Implicits._
import akka.stream.ClosedShape
import java.nio.file.{Path, Paths}

import com.github.jonnylaw.model._
import com.github.jonnylaw.model.POMP.{PoissonModel, SeasonalModel, LinearModel, BernoulliModel}
import com.github.jonnylaw.model.DataTypes._
import com.github.jonnylaw.model.{State, Model}
import com.github.jonnylaw.model.SimData._
import com.github.jonnylaw.model.Utilities._
import com.github.jonnylaw.model.State._
import com.github.jonnylaw.model.Parameters._
import com.github.jonnylaw.model.StateSpace._
import com.github.jonnylaw.model.Streaming._
import java.io.{PrintWriter, File}
import breeze.stats.distributions.{Gaussian, MultivariateGaussian}
import breeze.linalg.{DenseVector, diag}
import breeze.numerics.exp
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._

/**
  * A model to use for the examples in this class
  */
trait PoissonModel {
  val p = LeafParameter(
    GaussianParameter(-1.0, 1.0),
    None,
    OrnsteinParameter(theta = 1.0, alpha = 0.05, sigma = 0.5))
  
  val model = PoissonModel(stepOrnstein)
}

/**
  * Simulate 100 observaitions from a simple bernoulli model
  */
object SimulatePoisson extends App {

  val mod = new PoissonModel {}
  
  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod.model(mod.p))

  val pw = new PrintWriter("PoissonSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}

/**
  * Filter observations as a batch, return the state and credible intervals
  */
object FilterPoisson extends App {
  // read in the data from a csv file and parse it to a Data object
  // without the state, eta and gamma
  val data = scala.io.Source.fromFile("PoissonSims.csv").getLines.
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
    toVector.
    sortBy(_.t)

  val mod = new PoissonModel {}
  
  val filter = Filter(mod.model, ParticleFilter.multinomialResampling)

  val filtered = filter.filterWithIntervals(data, 0.0)(1000)(mod.p)

  val pw = new PrintWriter("PoissonFiltered.csv")
  pw.write(filtered.mkString("\n"))
  pw.close()
}

/**
  * An example showing real time filtering of observations arriving as a stream
  */
object FilterPoissonOnline extends App {
  implicit val system = ActorSystem("FilterOnline")
  implicit val materializer = ActorMaterializer()

  val model = new PoissonModel {}
  val mod = model.model(model.p)
  val observations = simStream(mod, 0, t0 = 0.0)

  // write the observations to a file
  observations.
    take(100).
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("OnlinePoisson.csv")))

  // particles and initial state for particle filter
  val n = 1000
  val t0 = 0.0 // replace with first time point
  val particleCloud = Vector.fill(n)(mod.x0.draw)

  val pf = Filter(model.model, ParticleFilter.multinomialResampling)
  
  // Use scan to filter a stream, which allows us to output the estimated state as the observations arrive
  pf.filter(observations, 0.0)(n)(model.p).
    drop(1). // drop the initial state, with no corresponding observation
    map(a => ByteString(s"$a\n")).
    runWith(FileIO.toFile(new File("filteredPoissonOnline.csv")))

  Thread.sleep(10000) // sleep for 10 seconds

  system.shutdown
}

object GetPoissonParameters {
  def main(args: Array[String]) = {
    implicit val system = ActorSystem("FilterOnline")
    implicit val materializer = ActorMaterializer()

    // Read in the data from a file and parse it to a vector of Data
    val data = scala.io.Source.fromFile("PoissonSims.csv").getLines.
      map(a => a.split(",")).
      map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
      toVector.sortBy(_.t)

    // parse supplied command line arguments for number of iterations, particles and
    // the size of the diagonal entries of the covariance matrix of the proposal distribution
    val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

    // create the model trait
    val mod = new PoissonModel {}

    // build the particle filter by selecting the model type and resampling scheme
    val filter = Filter(mod.model, ParticleFilter.multinomialResampling)

    // specify the filter type (llFilter, to return estimate of log-likelihood),
    // the number of particles and observations
    val mll = filter.llFilter(data, 0.0)(particles) _

    // build the PMMH algorithm using mll estimate (via particle filter), the
    // initial parameters and the proposal distribution for new paramters
    val mh = ParticleMetropolis(mll, mod.p, Parameters.perturb(delta))

    // run the PMMH as an akka stream in parallel and write the results to a file
    runPmmhToFile(s"PoissonSimParams-$delta-$particles", 2,
      mod.p, mll, Parameters.perturb(delta), iters)
  }
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

object SimulateSeasonal extends App {
  val params: Parameters = LeafParameter(
    GaussianParameter(DenseVector(Array.fill(6)(0.0)),
      diag(DenseVector(Array.fill(6)(1.0)))),
    Some(1.0),
    BrownianParameter(DenseVector.fill(6)(0.1), diag(DenseVector.fill(6)(1.0))))

  val mod = SeasonalModel(24, 3, stepBrownian)

  val times = (1 to 100).map(_.toDouble).toList
  val sims = simData(times, mod(params))

  val pw = new PrintWriter("SeasonalSims.csv")
  pw.write(sims.mkString("\n"))
  pw.close()
}
