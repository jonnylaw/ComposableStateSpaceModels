// package com.github.jonnylaw.examples

// import akka.stream.scaladsl._
// import akka.stream._
// import akka.actor.ActorSystem
// import akka.util.ByteString

// import breeze.linalg.{DenseVector, DenseMatrix}
// import breeze.linalg.cholesky
// import breeze.stats.distributions.{Gamma, Gaussian, Rand}
// import breeze.numerics.{exp, log}
// import cats._
// import cats.data.Kleisli
// import cats.implicits._
// import com.github.jonnylaw.model._
// import java.nio.file.Paths
// import scala.collection.parallel.immutable.ParVector
// import scala.concurrent.Future
// import scala.concurrent.ExecutionContext.Implicits.global
// import spray.json._

// trait SeasonalTestModel {
//   val seasonalParam: Parameters = Parameters.leafParameter(
//     Some(3.0),
//     SdeParameter.brownianParameter(
//       m0 = 0.5,
//       c0 = 0.12,
//       mu = 0.1,
//       sigma = 0.5)
//   )

//   val mod = Model.seasonalModel(24, 1, Sde.brownianMotion(2))

//   implicit val system = ActorSystem("SeasonalModel")
//   implicit val materializer = ActorMaterializer()
// }

// object SimSeasonalModel extends App with SeasonalTestModel {
//   SimulateData(mod(seasonalParam)).
//     simRegular(1.0).
//     take(500).
//     map((d: Data) => d.show).
//     runWith(Streaming.writeStreamToFile("data/SeasonalModelSims.csv")).
//     onComplete(_ => system.terminate())
// }

// /** Filter from the start of the series with the parameters used to simulate the model **/
// object FilterSeasonal extends App with SeasonalTestModel {
//   val data = DataFromFile("data/SeasonalModelSims.csv").
//     observations

//   val t0 = 0.0

//   val filter = ParticleFilter.filter(Resampling.systematicResampling, t0, 1000)

//   data.
//     via(filter(mod(seasonalParam))).
//     map(ParticleFilter.getIntervals(mod(seasonalParam))).
//     drop(1).
//     map(_.show).
//     runWith(Streaming.writeStreamToFile("data/SeasonalModelFiltered.csv")).
//     onComplete(_ => system.terminate())
// }

// /** Determine the appropriate amount of particles in the particle filter for the seasonal model **/
// object SeasonalPilotRun extends App with SeasonalTestModel {
//   val particles = Vector(100, 200, 500, 1000, 2000)
//   val resample: Resample[State] = Resampling.systematicResampling _

//   val res = for {
//     data <- DataFromFile("data/SeasonalModelSims.csv").observations.runWith(Sink.seq)
//     vars = Streaming.pilotRun(data.toVector, mod, seasonalParam, resample, particles, 100)
//     io <- vars.map { case (n, v) => s"$n, $v" }.
//       runWith(Streaming.writeStreamToFile("data/SeasonalPilotRun.csv"))
//   } yield io

//   res.onComplete(_ => system.terminate())
// }

// /** Determine the parameters of the seasonal model using PMMH **/
// object DetermineSeasonalParameters extends App with SeasonalTestModel with DataProtocols {

//   // specify a prior distribution on the parameters
//   def prior(p: Parameters) = { (p: @unchecked) match {
//     case LeafParameter(Some(v), BrownianParameter(m, c, mu, sigma)) =>
//       Gamma(0.5, 1.0).logPdf(exp(v)) +
//       Gaussian(0.5, 3.0).logPdf(m) + 
//       Gamma(0.05, 2.0).logPdf(exp(c)) + 
//       Gaussian(0.1, 3.0).logPdf(mu) + 
//       Gamma(0.25, 2.0).logPdf(exp(sigma))
//   }}

//   // specify the resampling scheme
//   def resample: Resample[State] = Resampling.systematicResampling _

//   // Specify the data to use in a batch run of the PMMH algorithm 
//   DataFromFile("data/SeasonalModelSims.csv").
//     observations.
//     take(400).
//     grouped(400).
//     mapConcat(data => (1 to 2).map((data, _))).
//     mapAsync(2) { case (data, chain) =>
//       val filter = ParticleFilter.llStateReader(data.toVector, resample, 300)
//       val pf: Kleisli[Rand, Parameters, (LogLikelihood, Vector[StateSpace])] = Kleisli {
//         p => filter(mod(seasonalParam))
//       }
//       val logTrans = (from: Parameters, to: Parameters) => 0.0
//       val pmmh = MetropolisHastings.pmmhState(seasonalParam,
//         Parameters.perturb(0.1), logTrans, prior)

//       pmmh(pf).
//         take(10000).
//         via(Streaming.monitorStateStream).
//         map(_.toJson.compactPrint).
//         runWith(Streaming.writeStreamToFile(s"data/SeasonalModelParams-$chain.json"))
//     }.
//     runWith(
//       Sink.onComplete(_ => system.terminate())
//     )
// }
