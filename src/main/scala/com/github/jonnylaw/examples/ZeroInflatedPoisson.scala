// package com.github.jonnylaw.examples

// import akka.stream.scaladsl._
// import akka.stream._
// import akka.util.ByteString
// import akka.actor.ActorSystem
// import breeze.linalg.{DenseVector, DenseMatrix}
// import breeze.linalg.cholesky
// import breeze.stats.distributions._
// import breeze.numerics.log
// import cats.implicits._
// import cats.data.Kleisli
// import com.github.jonnylaw.model._
// import java.nio.file.Paths
// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.concurrent._
// import spray.json._

// trait ZipModel {
//   val p = Parameters.leafParameter(
//     Some(log(0.2)),
//     SdeParameter.ouParameter(m0 = 0.0, c0 = log(1.0), theta = 2.0, alpha = log(0.25), log(0.5)))

//   val model = Model.zeroInflatedPoisson(Sde.ouProcess(1))

//   // actor system
//   implicit val system = ActorSystem("ZipModel")
//   implicit val materializer = ActorMaterializer()

//   val prior = (p: Parameters) => 0.0
// }

// object SimZipModel extends App with ZipModel with DataProtocols {
//   SimulateData(model(p)).
//     observations.
//     take(500).
//     map(_.toJson.compactPrint).
//     runWith(Streaming.writeStreamToFile("data/ZiPoissonModel.json")).
//     onComplete(_ => system.terminate())
// }

// object PilotRunZip extends App with ZipModel with DataProtocols {

//   val res = for {
//     data <- DataFromJson("data/ZiPoissonModel.json").
//       observations.
//       take(400).
//       runWith(Sink.seq)
//     particles = Vector(100, 200, 500, 1000, 2000)
//     resampling: Resample[State] = Resampling.stratifiedResampling _
//     vars = Streaming.pilotRun(data.toVector, model, p, resampling, particles, 100)
//     io <- vars.
//       map { case (n, v) => s"$n, $v" }.
//       runWith(Streaming.writeStreamToFile("data/PilotRunZip.csv"))
//   } yield io

//   res.
//     onComplete(_ => system.terminate())
// }

// object ZipModelPosterior extends App with ZipModel with DataProtocols {
//   val resample: Resample[State] = Resampling.stratifiedResampling _

//   DataFromJson("data/ZiPoissonModel.json").
//     observations.
//     take(400).
//     grouped(400).
//     mapConcat(d => (1 to 2).map(i => (d, i))).
//     mapAsync(2) { case (data, chain) => // run two chains in parallel
//       val filter = ParticleFilter.llStateReader(data.toVector, resample, 100)
//       val pf: Kleisli[Rand, Parameters, (LogLikelihood, Vector[StateSpace])] = Kleisli {
//         p => filter(model(p))
//       }
//       val logTrans = (from: Parameters, to: Parameters) => 0.0
//       val pmmh = MetropolisHastings.pmmhState(p, Parameters.perturb(0.1), logTrans, prior)

//       pmmh(pf).
//         via(Streaming.monitorStateStream).async.
//         take(10000).
//         map(_.toJson.compactPrint).
//         runWith(Streaming.writeStreamToFile(s"data/ZiPoissonPosterior-$chain.json"))
//     }.
//     runWith(Sink.onComplete(_ => system.terminate()))
// }
