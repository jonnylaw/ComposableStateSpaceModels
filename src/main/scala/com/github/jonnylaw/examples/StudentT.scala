package com.github.jonnylaw.examples

import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem
import akka.util.ByteString

import com.github.jonnylaw.model._
import java.nio.file.Paths
import breeze.linalg.{DenseVector, DenseMatrix, diag}
import cats._
import cats.implicits._

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait TModel {
  val tparams = Parameters.leafParameter(
    Some(0.3),
    SdeParameter.ouParameter(0.0, 3.0, 3.0, 1.0, 0.5))
  val seasParams: Parameters = Parameters.leafParameter(
    None,
    SdeParameter.ouParameter(0.0, 3.0, theta = 2.0,  alpha = 0.5, sigma = 0.3))

  val p = tparams |+| seasParams

  val st = Model.studentsTModel(Sde.ouProcess(1), 5)
  val seasonal = Model.seasonalModel(24, 3, Sde.ouProcess(6))

  val unparamMod = st |+| seasonal
  val mod = unparamMod(p)

  implicit val system = ActorSystem("StudentT")
  implicit val materializer = ActorMaterializer()
}

object SeasStudentT extends App with TModel {
  // simulate hourly data, with some missing
  val times = (1 to 7*24).
    map(_.toDouble).
    filter(_ => scala.util.Random.nextDouble < 0.95)

  // simulate from the Student T POMP model, simulating states and observations at the times above
  Source.apply(times).
    via(SimulateData(mod).simPompModel(0.0)).
    map((x: Data) => x.show).
    runWith(Streaming.writeStreamToFile("data/SeasTSims.csv")).
    onComplete(_ => system.terminate())
}

object GetSeasTParams extends App with TModel {
  // specify the prior distribution over the parameters 
  def prior: Parameters => LogLikelihood = p => 0.0

  // read the data in
  // create the marginal likelihood, using a particle filter
  // create a stream of MCMC iterations and run two in parallel
  Source(Vector(1, 2)).
    mapAsync(2) { (chain: Int) =>
      for {
        data <- DataFromFile("data/SeasTSims.csv").observations.runWith(Sink.seq)
        mll = (p: Parameters) => ParticleFilter.likelihood(data.toVector, Resampling.systematicResampling _, 500)(unparamMod(p))
        pmmh = ParticleMetropolisSerial(mll, p, Parameters.perturb(0.05), prior)
        io <- pmmh.params.take(10000).map(_.show).runWith(Streaming.writeStreamToFile(s"data/seastMCMC-$chain.csv"))
      } yield io
    }.
    runWith(Sink.onComplete(_ => system.terminate))
}
