package com.github.jonnylaw.examples

import breeze.stats.distributions.{Gaussian, Gamma}
import cats.data.Reader
import cats.implicits._
import com.github.jonnylaw.model._
import breeze.linalg.DenseVector
import fs2._
import java.nio.file.Paths
import java.io.File

trait LgcpModel {
  /** Define the model **/
  val params = Parameters.leafParameter(
    None,
    OrnsteinParameter(DenseVector(2.0),
      DenseVector(1.0),
      theta = DenseVector(0.1),
      alpha = DenseVector(0.4),
      sigma = DenseVector(0.5)))

  val model = Model.lgcpModel(Sde.ornsteinUhlenbeck)
}

object SimulateLGCP extends App with LgcpModel {
  new File("data/lgcpsims.csv").delete()

  Stream.emits[Task, Data](SimulatedData(model(params)).
    simLGCP(0.0, 10.0, 2)).
    map((d: Data) => d.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/lgcpsims.csv"))).
    run.
    unsafeRun()
}

object FilteringLgcp extends App with LgcpModel {
  new File("data/lgcpfiltered.csv").delete()
  val pf = FilterLgcp(model(params), ParticleFilter.multinomialResampling, 3)

  val data = io.file.readAll[Task](Paths.get("data/lgcpsims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun().
    sortBy(_.t). // ensure the data is sorted
    drop(1) 

  Stream.emits[Task, PfState[Vector]](pf.accFilter(data.toVector, 0.0)(1000)).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/lgcpfiltered.csv"))).
    run.
    unsafeRun()
}

object GetLgcpParams extends App with LgcpModel {
  // read in the LGCP simulated data using fs2, make it a vector and sort it by timestamp
  val data = io.file.readAll[Task](Paths.get("data/lgcpsims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun().
    sortBy(_.t)

  // the higher this value, the higher the accuracy of the calculated cumulative hazard 
  val precision = 1
  val t0 = data.map(_.t).min // starting time of the data

  val filter = Reader { (mod: Model) => FilterLgcp(mod, ParticleFilter.multinomialResampling, precision).
    llFilter(t0, 200)(data.toVector)}

  val iterations = 10000

  val mll = filter.compose(model)

  def prior = (p: Parameters) => p match {
    case LeafParameter(None, OrnsteinParameter(m, c, t, a, s)) =>
      Gaussian(0.0, 10.0).logPdf(m(0)) + 
      Gamma(1.0, 1.0).logPdf(1/c(0)) +
      Gaussian(2.0, 10.0).logPdf(1/t(0)) +
      Gamma(2.0, 1.0).logPdf(1/a(0)) +
      Gamma(1.0, 1.0).logPdf(1/s(0))
    case _ => throw new Exception
  }

  ParticleMetropolis(mll.run, params, Parameters.perturb(0.05), prior).
    iters[Task].
    take(iterations).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/lgcpMCMC.csv"))).
    run.
    unsafeRun()
}
