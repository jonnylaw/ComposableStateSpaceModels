// package com.github.jonnylaw.examples

// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.stream.scaladsl.Source
// import scala.concurrent.ExecutionContext.Implicits.global
// import java.nio.file.Paths
// import akka.stream.scaladsl._
// import scala.concurrent.{duration, Await}
// import scala.concurrent.duration._
// import akka.util.ByteString

// import com.github.jonnylaw.model._
// import Streaming._
// import ParticleFilter._
// import DataTypes._
// import Model._
// import Utilities._
// import State._
// import Parameters._
// import StateSpace._
// import java.io.{PrintWriter, File}
// import breeze.stats.distributions.Gaussian
// import breeze.linalg.{DenseVector, diag}

// trait LgcpModel {
//   /** Define the model **/
//   val params = LeafParameter(
//     GaussianParameter(0.0, 1.0),
//     None,
//     OrnsteinParameter(2.0, 0.5, 1.0))

//   val model = LogGaussianCox(stepOrnstein)
// }

// object SimulateLGCP extends App with LgcpModel {
//   val sims = simLGCP(0.0, 10.0, model(params), 2)

//   val pw = new PrintWriter("lgcpsims.csv")
//   pw.write(sims.mkString("\n"))
//   pw.close()
// }

// object FilteringLgcp extends App with LgcpModel {
//   implicit val system = ActorSystem("FilterLgcp")
//   implicit val materializer = ActorMaterializer()

//   val data = FileIO.fromPath(Paths.get("lgcpsims.csv")).
//     via(Framing.delimiter(
//       ByteString("\n"),
//       maximumFrameLength = 256,
//       allowTruncation = true)).
//     map(_.utf8String).
//     map(a => a.split(",")).
//     map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

//   val pf = FilterLgcp(model, ParticleFilter.multinomialResampling, 2)

//   data.
//     via(pf.filter(0.0)(1000)(params)).
//     map(s => ByteString(s + "\n")).
//     runWith(FileIO.toPath(Paths.get("lgcpfiltered.csv"))).
//     onComplete(_ => system.terminate)
// }

// object GetLgcpParams extends App with LgcpModel {
//   implicit val system = ActorSystem("GetLgcpParams")
//   implicit val materializer = ActorMaterializer()

//   val data = FileIO.fromPath(Paths.get("lgcpsims.csv")).
//     via(Framing.delimiter(
//       ByteString("\n"),
//       maximumFrameLength = 256,
//       allowTruncation = true)).
//     map(_.utf8String).
//     map(a => a.split(",")).
//     map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

//   val filter = FilterLgcp(model, ParticleFilter.multinomialResampling, 2)


//   val iterations = 10000
//   val delta = args.map(_.toDouble).toVector

//   // the PMMH algorithm is defined as an Akka stream,
//   // this means we can write the iterations to a file as they are generated
//   // therefore we use constant time memory even for large MCMC runs

//   data.
//     take(100).
//     grouped(100).
//     map(d => {
//       val mll = filter.llFilter(d.sortBy(_.t).toVector, d.map(_.t).min)(200) _

      
//       ParticleMetropolis(mll, params, Parameters.perturbIndep(delta)).
//         iters.
//         take(iterations).
//         map(s => ByteString(s + "\n")).
//         runWith(FileIO.toPath(Paths.get("LgcpMCMC.csv"))).
//         onComplete(_ => system.terminate)
//     }).
//     runWith(Sink.ignore)
// }
