// package com.github.jonnylaw.examples

// import java.nio.file.Paths
// import fs2._

// import com.github.jonnylaw.model._
// import breeze.stats.distributions.Gaussian
// import breeze.linalg.{DenseVector, diag}
// import java.io.PrintWriter

// trait LgcpModel {
//   /** Define the model **/
//   val params = Parameters.leafParameter(
//     None,
//     OrnsteinParameter(DenseVector(0.0),
//       DenseMatrix((1.0)),
//       DenseVector(2.0),
//       DenseVector(0.5),
//       DenseVector(1.0)))

//   val model = Model.logGaussianCox(Sde.ornsteinUhlenbeck)
// }

// object SimulateLGCP extends App with LgcpModel {
//   val sims = SimulatedData(model(params)).
//     simLGCP(0.0, 10.0, 2)

//   val pw = new PrintWriter("data/lgcpsims.csv")
//   pw.write(sims.mkString("\n"))
//   pw.close()
// }

// object FilteringLgcp extends App with LgcpModel {
//   val data = io.file.readAll[Task](Paths.get("data/lgcpsims.csv"), 4096).
//     through(text.utf8Decode).
//     through(text.lines).
//     map(a => a.split(", ")).
//     map(d => TimedObservation(d(0).toDouble, d(1).toDouble))

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
