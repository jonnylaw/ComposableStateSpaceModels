// package com.github.jonnylaw.examples

// import akka.stream.scaladsl._
// import akka.stream._
// import akka.actor.ActorSystem
// import akka.util.ByteString


// import breeze.linalg.{DenseVector, DenseMatrix}
// import breeze.linalg.eigSym
// import breeze.stats.distributions._
// import cats.data.Reader
// import cats.implicits._
// import cats._

// import com.github.jonnylaw.model._
// import Parameters._

// import java.nio.file.Paths
// import java.io._

// import scala.collection.parallel.immutable.ParVector
// import scala.concurrent.ExecutionContext.Implicits.global
// import scala.concurrent.Future
// import spray.json._

// /**
//   * Define a model to use throughout the examples in this file
//   */
// trait TestModel {
//   val poissonParams = Parameters.leafParameter(None, SdeParameter.brownianParameter(0.0, 1.0, 0.01, 0.3))

//   val seasonalParams = Parameters.leafParameter(None, SdeParameter.ouParameter(1.0, 1.0, 1.0, 0.5, 0.3))

//   val params = poissonParams |+| seasonalParams
//   val poisson = Model.poissonModel(Sde.brownianMotion(1))
//   val seasonal = Model.seasonalModel(24, 3, Sde.ouProcess(6))
//   val model = poisson |+| seasonal

//   // choose a prior for the parameter estimation
//   def priorDrift(p: Parameters) = p match {
//     case LeafParameter(_, BrownianParameter(m0, c0, mu, sigma)) =>
//       Some(
//         Gaussian(1.0, 3.0).logPdf(m0) +
//           Gamma(0.5, 2.0).logPdf(c0) +
//           Gaussian(0.0, 3.0).logPdf(mu) +
//           Gamma(0.1, 3.0).logPdf(sigma))
//     case _ => None
//   }

//   def priorDaily(p: Parameters) = p match {
//     case LeafParameter(_, OrnsteinParameter(m0, c0, theta, alpha, sigma)) =>
//       Some(
//         m0.mapValues(m => Gaussian(-0.5, 3.0).logPdf(m)).reduce(_+_) +
//           c0.mapValues(c => Gamma(0.3, 3.0).logPdf(c)).reduce(_+_) +
//           theta.mapValues(m => Gaussian(-1.0, 3.0).logPdf(m)).reduce(_+_) +
//           alpha.mapValues(a => Gamma(0.15, 3.0).logPdf(a)).reduce(_+_) +
//           sigma.mapValues(s => Gamma(1.0, 3.0).logPdf(s)).reduce(_+_))
//     case _ => None
//   }

//   def prior = (p: Parameters) => p match {
//     case BranchParameter(drift, daily) =>
//       for {
//         p1 <- priorDrift(drift)
//         p2 <- priorDaily(daily)
//       } yield p1 + p2
//     case _ => None
//   }

//   // Actor system is required to run the streaming examples
//   implicit val system = ActorSystem("ComposedModel")
//   implicit val materializer = ActorMaterializer()
// }

// /**
//   * Simulate a poisson model, with seasonal rate parameter
//   */
// object SimulateSeasonalPoisson extends App with TestModel {
//   val times = (0.0 to 240.0 by 1.0).
//     filter(a => scala.util.Random.nextDouble < 0.9). // 10% of data missing
//     toList

//   Source.apply(times).
//     via(SimulateData(model(params)).simPompModel(0.0)).
//     map((x: Data) => x.show).
//     runWith(Streaming.writeStreamToFile("data/SeasonalPoissonSims.csv")).
//     onComplete(_ => system.terminate())
// }

// object PilotRunComposed extends App with TestModel {
//   val particles = Vector(100, 200, 500, 1000, 2000, 5000)
//   val resample: Resample[State, Id] = Resampling.systematicResampling _

//   val res = for {
//     data <- DataFromFile("data/SeasonalPoissonSims.csv").observations.runWith(Sink.seq)
//     vars = Streaming.pilotRun(data.toVector, model, params, resample, particles, 100)
//     io <- vars.map { case (n, v) => s"$n, $v" }.
//       runWith(Streaming.writeStreamToFile("data/ComposedPilotRun.csv"))
//   } yield io

//   res.onComplete(_ => system.terminate())
// }

// object DetermineComposedParams extends TestModel with DataProtocols {
//   def main(args: Array[String]) = {
//     val scale = args.head.toDouble
//     val sigma = DenseMatrix.eye[Double](34)

//     // 1. read data from file
//     // 2. compose model with particle filter to get function from Parameters => LogLikelihood
//     // 3. Build the PMMH algorithm using the mll estimate from the particle filter
//     // 4. write it to a file as a stream
//     def iters(scale: Double)(chain: Int) = for {
//       data <- DataFromFile("data/SeasonalPoissonSims.csv").observations.runWith(Sink.seq)
//       filter = (p: Parameters) => ParticleFilter.likelihood(data.sortBy(_.t).toVector, Resampling.systematicResampling, 250)(model(p))
//       pmmh = ParticleMetropolisSerial(filter, params, Parameters.perturbMvnEigen(eigSym(sigma), scale), p => prior(p).get)
//       io <- pmmh.
//         params.
//         take(100000).
//         via(Streaming.monitorStream).
//         map(_.toJson.compactPrint).
//         runWith(Streaming.writeStreamToFile(s"data/seasonalPoissonParams-$chain.json"))
//     } yield io

//     Future.sequence((1 to 2).map(iters(0.25))).
//       onComplete(_ => system.terminate())
//   }
// }

// object FilterSeasonalPoisson extends TestModel {
//   def main(args: Array[String]): Unit = {
//     val n = 1000 // number of particles for the particle filter
//     val t0 = 0.0 // starting time of the filter (time of the first observation)
//     setParallelismGlobally(8) // set the number of threads to use for the parallel particle filter

//     val resample: Resample[State, Id] = Resampling.multinomialResampling

//     val filter = ParticleFilter.filter(resample, 0.0, n)

//     DataFromFile("data/SeasonalPoissonSims.csv").
//       observations.
//       via(filter(model(params))).
//       map(ParticleFilter.getIntervals(model(params))).
//       drop(1).
//       map(_.show).
//       runWith(Streaming.writeStreamToFile("data/SeasonalPoissonFiltered.csv")).
//       onComplete(_ => system.terminate())
//   }
// }
