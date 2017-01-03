// package com.github.jonnylaw.examples

// import akka.actor.ActorSystem
// import akka.stream.ActorMaterializer
// import akka.NotUsed
// import akka.stream._
// import scaladsl._
// import akka.util.ByteString
// import GraphDSL.Implicits._
// import java.nio.file.{Path, Paths}
// import scala.concurrent.duration._

// import com.github.jonnylaw.model._
// import DataTypes._
// import Model._
// import State._
// import Parameters._
// import StateSpace._
// import Streaming._
// import java.io.{PrintWriter, File}
// import breeze.stats.distributions.{Gaussian, MultivariateGaussian}
// import breeze.linalg.{DenseVector, diag}
// import breeze.numerics.exp
// import scala.concurrent.ExecutionContext.Implicits.global
// import cats.implicits._

// /**
//   * A model to use for the examples in this class
//   */
// trait PoissonModel {
//   val p = LeafParameter(
//     GaussianParameter(-1.0, 1.0),
//     None,
//     OrnsteinParameter(theta = 1.0, alpha = 0.05, sigma = 0.5))
  
//   val model: Parameters => Model = PoissonModel(stepOrnstein)
// }

// /**
//   * Simulate 100 observaitions from a simple bernoulli model
//   */
// object SimulatePoisson extends App with PoissonModel {
//   implicit val system = ActorSystem("SimulatePoisson")
//   implicit val materializer = ActorMaterializer()
  
//   val times: Source[Time, NotUsed] = Source((1 to 100).map(_.toDouble))

//   times.
//     via(model(p).simPompModel(1.0)).
//     map(s => ByteString(s + "\n")).
//     runWith(FileIO.toPath(Paths.get("PoissonSims.csv"))).
//     onComplete(_ => system.terminate)
// }

// /**
//   * Filter observations as a batch, return the state and credible intervals
//   */
// object FilterPoisson extends App with PoissonModel {
//   implicit val system = ActorSystem("FilterPoisson")
//   implicit val materializer = ActorMaterializer()
  
//   // read in the data from a csv file as a stream and parse it to a Data object
//   // without the state, eta and gamma
//   val data = FileIO.fromPath(Paths.get("PoissonSims.csv")).
//     via(Framing.delimiter(
//       ByteString("\n"),
//       maximumFrameLength = 256,
//       allowTruncation = true)).
//     map(_.utf8String).
//     map(line => line.split(",")).
//     drop(1).
//     map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

//   val pf = Filter(model, ParticleFilter.multinomialResampling)

//   data.
//     via(pf.filter(0.0)(1000)(p)).
//     map(s => ByteString(s + "\n")).
//     runWith(FileIO.toPath(Paths.get("PoissonFiltered.csv"))).
//     onComplete(_ => system.terminate)
// }

// object GetPoissonParameters {
//   def main(args: Array[String]) = {
//     implicit val system = ActorSystem("FilterOnline")
//     implicit val materializer = ActorMaterializer()

//     // Read in the data from a file and parse it to a vector of Data
//     val data = scala.io.Source.fromFile("PoissonSims.csv").getLines.
//       map(a => a.split(",")).
//       map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None)).
//       toVector.sortBy(_.t)

//     // parse supplied command line arguments for number of iterations, particles and
//     // the size of the diagonal entries of the covariance matrix of the proposal distribution
//     val (iters, particles, delta) = (args.head.toInt, args(1).toInt, args(2).toDouble)

//     // create the model trait
//     val mod = new PoissonModel {}

//     // build the particle filter by selecting the model type and resampling scheme
//     val filter = Filter(mod.model, ParticleFilter.multinomialResampling)

//     // specify the filter type (llFilter, to return estimate of log-likelihood),
//     // the number of particles and observations
//     val mll = filter.llFilter(data, 0.0)(particles) _

//     // build the PMMH algorithm using mll estimate (via particle filter), the
//     // initial parameters and the proposal distribution for new paramters
//     val mh = ParticleMetropolis(mll, mod.p, Parameters.perturb(delta))

//     // run the PMMH as an akka stream in parallel and write the results to a file
//     runPmmhToFile(s"PoissonSimParams-$delta-$particles", 2,
//       mod.p, mll, Parameters.perturb(delta), iters)
//   }
// }

// /**
//   * Simulate a brownian motion state space 
//   */
// object SimulateBrownian extends App {
//   implicit val system = ActorSystem("SimBrownian")
//   implicit val materializer = ActorMaterializer()

//   val p = BrownianParameter(DenseVector(0.1, 0.1), diag(DenseVector(0.1, 0.5)))
//   val x0: State = LeafState(MultivariateGaussian(DenseVector(1.0, 1.0), diag(DenseVector(5.0, 5.0))).draw)
//   val dt = 0.1

//   Source.unfold(x0)(x => Some((stepBrownian(p)(x, dt).draw, x))).
//     zip(Source.tick(1 second, 1 second, Unit)).
//     map{ case (a, _) => a }.
//     take(100).
//     runForeach(println).
//     onComplete(_ => system.terminate)
// }

// /**
//   * Simulate an Ornstein-Uhlenbeck state space
//   */
// object SimulateOrnstein extends App {
//   implicit val system = ActorSystem("SimOrnstein")
//   implicit val materializer = ActorMaterializer()

//   val p = OrnsteinParameter(theta = 1.0, alpha = 0.05, sigma = 1.0)
//   val x0: State = LeafState(DenseVector(Gaussian(6.0, 1.0).draw))
//   val dt = 0.1

//   Source.unfold(x0)(x => Some((stepOrnstein(p)(x, dt).draw, x))).
//     map(a => ByteString(a + "\n")).
//     runWith(FileIO.toPath(Paths.get("OrnsteinSims.csv"))).
//     onComplete(_ => system.terminate)
// }

// object SimulateSeasonal extends App {
//   implicit val system = ActorSystem("SimSeasonal")
//   implicit val materializer = ActorMaterializer()

//   val params: Parameters = LeafParameter(
//     GaussianParameter(DenseVector(Array.fill(6)(0.0)),
//       diag(DenseVector(Array.fill(6)(1.0)))),
//     Some(1.0),
//     BrownianParameter(DenseVector.fill(6)(0.1), diag(DenseVector.fill(6)(1.0))))

//   val mod = SeasonalModel(24, 3, stepBrownian)

//   val times = Source((1 to 100).map(_.toDouble))

//   times.
//     via(mod(params).simPompModel(1.0)).
//     map(s => ByteString(s + "\n")).
//     runWith(FileIO.toPath(Paths.get("SeasonalSims.csv"))).
//     onComplete(_ => system.terminate)
// }
