package com.github.jonnylaw.examples

import com.github.jonnylaw.model._
import java.nio.file.Paths
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import cats.implicits._
import fs2.{io, text, Task, Stream}

trait TModel {
  val tparams = Parameters.leafParameter(
    Some(0.3),
    SdeParameter.ornsteinParameter(
      DenseVector(0.0), 
      DenseVector((3.0)), 
      DenseVector(3.0), 
      DenseVector(1.0), 
      DenseVector(0.5)))
  val seasParams: Parameters = Parameters.leafParameter(
    None,
    SdeParameter.ornsteinParameter(
      DenseVector.fill(6)(0.0), 
      DenseVector.fill(6)(3.0),
      theta = DenseVector.fill(6)(2.0), 
      alpha = DenseVector.fill(6)(0.5),
      sigma = DenseVector.fill(6)(0.3)))

  val p = tparams |+| seasParams

  val st = Model.studentsTModel(Sde.ornsteinUhlenbeck, 5)
  val seasonal = Model.seasonalModel(24, 3, Sde.ornsteinUhlenbeck)

  val unparamMod = st |+| seasonal
  val mod = unparamMod(p)
}

object SeasStudentT extends App with TModel {

  // simulate hourly data, with some missing
  val times = (1 to 7*24).
    map(_.toDouble).
    filter(_ => scala.util.Random.nextDouble < 0.95).
    toList

  // simulate from the Student T POMP model, simulating states and observations at the times above
  Stream.emits[Task, Double](times).
    through(SimulatedData(mod).simPompModel(0.0)).
    map((x: Data) => x.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get("data/SeasTSims.csv"))).
    run.
    unsafeRun
}

object GetSeasTParams extends App with TModel {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  // read the data in as a Vector
  val data: Vector[Data] = io.file.readAll[Task](Paths.get("data/SeasTSims.csv"), 4096).
    through(text.utf8Decode).
    through(text.lines).
    map(a => a.split(", ")).
    map(d => TimedObservation(d(0).toDouble, d(1).toDouble)).
    runLog.
    unsafeRun

  // create the marginal likelihood, using a particle filter
  val mll = ParticleFilter.likelihood(data.toList, 200, ParticleFilter.multinomialResampling) compose unparamMod

  // specify the prior distribution over the parameters 
  def prior: Parameters => LogLikelihood = p => 0.0

  // create a stream of MCMC iterations
  def iters(chain: Int) = ParticleMetropolis(mll.run, p, Parameters.perturb(0.05), prior).
    iters[Task].
    take(10000).
    map(_.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get(s"data/seastMCMC-$chain.csv"))).
    run

  // run four chains in parallel
  Task.parallelTraverse((1 to 4))(iters)
}
