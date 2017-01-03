// package com.github.jonnylaw.examples

// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.stream.IOResult
// import java.io.{File, PrintWriter}
// import akka.stream.scaladsl._
// import akka.util.ByteString
// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.concurrent.Future
// import java.nio.file.Paths

// import com.github.jonnylaw.model._
// import DataTypes._
// import SimData._
// import StateSpace._
// import Streaming._

// trait LinearModel {
//   val unparamMod = LinearModel(stepBrownian)
//   val p = LeafParameter(GaussianParameter(0.1, 1.0), Some(1.0), BrownianParameter(-0.2, 1.0))
//   val mod = unparamMod(p)
//   val filter = Filter(unparamMod, ParticleFilter.multinomialResampling)
// }

// /**
//   * Simulate from the linear model
//   */
// object SimLinear extends App with LinearModel {
//   implicit val system = ActorSystem("SimulateLinearModel")
//   implicit val materializer = ActorMaterializer()

//   mod.simRegular(1.0).
//     take(1000).
//     map(s => ByteString(s + "\n")).
//     runWith(FileIO.toPath(Paths.get("LinearModelSims.csv"))).
//     onComplete(_ => system.terminate)
// }

// /**
//   * Random walk Metropolis Hastings
//   * with different values of delta
//   */
// object BreezeMCMC extends App with LinearModel {
//   implicit val system = ActorSystem("PMMH")
//   implicit val materializer = ActorMaterializer()

//   val dn = for {
//     d <- List(0.05, 0.1, 0.25)
//     n <- List(200, 500, 1000)
//   } yield (d, n)

//   // Read in the data
//   val data: Source[Data, Future[IOResult]] = FileIO.fromPath(Paths.get("LinearModelSims.csv")).
//     via(Framing.delimiter(
//       ByteString("\n"),
//       maximumFrameLength = 256,
//       allowTruncation = true)).
//     map(_.utf8String).
//     map(a => a.split(",")).
//     map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))


//   Source(dn).mapAsync(parallelism = 8){ case (delta, n) =>

//     val mll: (Vector[Data], Int) => Parameters => LogLikelihood = (data, n) => filter.llFilter(data, 0.0)(n) _
    
//     data.
//       take(100).
//       grouped(100).
//       map((d: Seq[Data]) =>

//         ParticleMetropolis(mll(d.toVector, n), p, Parameters.perturb(delta)).
//           iters.
//           take(10000).
//           map(s => ByteString(s + "\n")).
//           runWith(FileIO.toPath(Paths.get(s"LinearModelPMMH_${delta}_${n}.csv"))).
//           onComplete(_ => system.terminate)
//       ).
//       runWith(Sink.ignore)
//   }
// }
