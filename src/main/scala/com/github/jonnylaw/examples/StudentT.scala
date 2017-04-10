// package com.github.jonnylaw.examples

// import akka.stream.scaladsl._
// import akka.stream._
// import akka.actor.ActorSystem
// import akka.util.ByteString
// import com.github.jonnylaw.model._
// import java.nio.file.Paths
// import breeze.linalg.{DenseVector, DenseMatrix, diag}
// import cats._
// import cats.data.Kleisli
// import cats.implicits._
// import scala.collection.parallel.immutable.ParVector
// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.concurrent.Future
// import spray.json._

// trait TModel {
//   val tparams = Parameters.leafParameter(
//     Some(0.3),
//     SdeParameter.ouParameter(0.0, 3.0, 3.0, 1.0, 0.5))
//   val seasParams: Parameters = Parameters.leafParameter(
//     None,
//     SdeParameter.ouParameter(0.0, 3.0, theta = 2.0,  alpha = 0.5, sigma = 0.3))

//   val p = tparams |+| seasParams

//   val st = Model.studentsTModel(Sde.ouProcess(1), 5)
//   val seasonal = Model.seasonalModel(24, 3, Sde.ouProcess(6))

//   val unparamMod = st |+| seasonal
//   val mod = unparamMod(p)

//   implicit val system = ActorSystem("StudentT")
//   implicit val materializer = ActorMaterializer()
// }

// object SeasStudentT extends App with TModel {
//   // simulate hourly data, with some missing
//   val times = (1 to 7*24).
//     map(_.toDouble).
//     filter(_ => scala.util.Random.nextDouble < 0.95)

//   // simulate from the Student T POMP model, simulating states and observations at the times above
//   Source.apply(times).
//     via(SimulateData(mod).simPompModel(0.0)).
//     map((x: Data) => x.show).
//     runWith(Streaming.writeStreamToFile("data/SeasTSims.csv")).
//     onComplete(_ => system.terminate())
// }

// object GetSeasTParams extends App with TModel with DataProtocols {
//   // specify the prior distribution over the parameters 
//   def prior: Parameters => LogLikelihood = p => 0.0
//   val resample: Resample[State] = Resampling.systematicResampling _

//   // read the data in
//   // create the marginal likelihood, using a particle filter
//   // create a stream of MCMC iterations and run two in parallel
//   DataFromFile("data/SeasTSims.csv").
//     observations.
//     take(400).
//     grouped(400).
//     mapConcat(d => (1 to 2).map((d,_))).
//     mapAsync(2) { case (d, chain) =>
//       val filter = ParticleFilter.llStateReader(d.toVector, resample, 100)
//       val pf = Kleisli { (p: Parameters) => filter(unparamMod(p)) }
//       val logTrans = (from: Parameters, to: Parameters) => 0.0
//       val pmmh = MetropolisHastings.pmmhState(p, Parameters.perturb(0.1), logTrans, prior)

//       pmmh(pf).
//         via(Streaming.monitorStateStream).async.
//         take(10000).
//         map(_.toJson.compactPrint).
//         runWith(Streaming.writeStreamToFile(s"data/seastMCMC-$chain.json"))
//     }.
//     runWith(Sink.onComplete(_ => system.terminate()))
// }
