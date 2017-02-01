package com.github.jonnylaw.examples

import java.nio.file.Paths
import com.github.jonnylaw.model._
import breeze.stats.distributions.{Gaussian, MultivariateGaussian, Gamma}
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import cats.implicits._
import fs2._

/**
  * A model to use for the examples in this class
  */
trait PoissonModel {
  val p = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      m0 = DenseVector(-1.0),
      c0 = DenseMatrix((1.0)),
      theta = DenseVector(1.0),
      alpha = DenseVector(0.05),
      sigma = DenseVector(0.5))
  )
  
  val model = Model.poissonModel(Sde.ornsteinUhlenbeck)
}

/**
  * Simulate 100 observaitions from a simple bernoulli model
  */
object SimulatePoisson extends App with PoissonModel {
  val times: Stream[Task, Time] = Stream.emits((1 to 100).map(_.toDouble))

  times.
    through(SimulatedData(model(p)).simPompModel(1.0)).
    map((d: Data) => d.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/PoissonSims.csv"))).
    run.
    unsafeRun
}

/**
  * Filter observations as a batch, return the state and credible intervals
  */
object FilterPoisson extends App with PoissonModel {
  // read in the data from a csv file as a stream and parse it to a Data object
  // without the state, eta and gamma
  val data = io.file.readAll[Task](Paths.get("data/PoissonSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

  val pf = Filter(model(p), ParticleFilter.multinomialResampling)

  data.
    through(pf.filter(0.0)(1000)).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/PoissonFiltered.csv"))).
    run.
    unsafeRun
}

object GetPoissonParameters extends App with PoissonModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

    // Read in the data from a file and parse it to a vector of Data
  val data = io.file.readAll[Task](Paths.get("data/PoissonSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun

  // parse supplied command line arguments for number of iterations, particles and
  // the size of the diagonal entries of the covariance matrix of the proposal distribution
  val (niters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

  // build the particle filter by selecting the model type and resampling scheme
  // specify the filter type (llFilter, to return estimate of log-likelihood),
  // the number of particles and observations
  val mll = ParticleFilter.likelihood(ParticleFilter.multinomialResampling, data, 200) compose model

  // specify the a prior distribution on the parameters
  def prior: Parameters => LogLikelihood = p => p match {
    case LeafParameter(_, OrnsteinParameter(m, v, t, a, s)) =>
      Gaussian(-1.0, 10.0).logPdf(m(0)) +
      Gamma(1.0, 10.0).logPdf(1/v.data(0)) +
      Gaussian(1.0, 10.0).logPdf(t(0)) +
      Gamma(1.0, 10.0).logPdf(1/a(0)) +
      Gamma(1.0, 10.0).logPdf(1/s(0))
  }

  // build the PMMH algorithm using mll estimate (via particle filter), the
  // initial parameters and the proposal distribution for new paramters
  def iters(chain: Int) = ParticleMetropolis(mll.run, p, Parameters.perturb(delta), prior).
    iters[Task].
    take(niters).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get(s"data/PoissonParameters-$chain.csv"))).
    run

  // run four chains in parallel
  Task.parallelTraverse((1 to 4))(iters)
}

/**
  * Simulate an SDE
  */
object SimulateBrownianMotion extends App {
  val p = SdeParameter.brownianParameter(
    DenseVector.fill(2)(1.0),
    DenseMatrix.eye[Double](2),
    DenseVector.fill(2)(-0.3),
    DenseMatrix.eye[Double](2) * 0.3)

  val p1 = SdeParameter.brownianParameter(
    DenseVector.fill(2)(1.0),
    DenseMatrix.eye[Double](2),
    DenseVector.fill(2)(0.3),
    DenseMatrix.eye[Double](2) * 0.3)

  val sde1 = Sde.brownianMotion(p)
  val sde2 = Sde.brownianMotion(p1)

  val composedSde = sde1 |+| sde2

  composedSde.
    simStream[Task](0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/brownianMotion.csv"))).
    run.
    unsafeRun
}

object SimOrnstein extends App {
  val p = SdeParameter.ornsteinParameter(
    DenseVector.fill(2)(0.0),
    diag(DenseVector.fill(2)(3.0)),
    DenseVector.fill(2)(2.0), DenseVector.fill(2)(0.5), DenseVector.fill(2)(0.3))
  val sde = Sde.ornsteinUhlenbeck

  sde(p).
    simStream[Task](0.0, 0.1).
    take(200).
    zipWithIndex.
    map { case (x, t) => t + ", " + x.show }.
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/ornsteinUhlenbeck.csv"))).
    run.
    unsafeRun
}

object SimulateSeasonal extends App {
  val params: Parameters = Parameters.leafParameter(
    Some(1.0),
    BrownianParameter(
      DenseVector.zeros[Double](6),
      DenseMatrix.eye[Double](6),
      DenseVector.fill(6)(0.1),
      diag(DenseVector.fill(6)(1.0))))

  val mod = Model.seasonalModel(24, 3, Sde.brownianMotion)

  val times = Stream.emits((1 to 100).map(_.toDouble))

  times.
    through(SimulatedData(mod(params)).simPompModel(1.0)).
    map((d: Data) => d.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/ornsteinUhlenbeck.csv"))).
    run.
    unsafeRun
}
