// package com.github.jonnylaw.examples

// import akka.stream.scaladsl._
// import akka.stream._
// import akka.actor.ActorSystem
// import breeze.linalg.{DenseVector, DenseMatrix, cholesky, eigSym}
// import breeze.numerics.{log, exp}
// import breeze.stats.distributions.{Gamma, Gaussian, Uniform, Rand}
// import cats._
// import cats.implicits._
// import com.github.jonnylaw.model._
// import scala.concurrent.ExecutionContext.Implicits.global
// import spray.json._
// import scala.concurrent._
// import scala.util.{Try, Success, Failure}
// import java.io.PrintWriter

// trait NegBinomialTest {
//   val negbinParams: Rand[Parameters] = {
//     for {
//       s <- Uniform(2.0, 6.0)
//       m0 <- Uniform(0.0, 0.01)
//       c0 <- Uniform(0.01, 0.02)
//       mu <- Rand.always(0.0)
//       sigma <- Uniform(0.000001, 0.00002)
//     } yield Parameters.leafParameter(
//       Some(log(s)),
//       SdeParameter.brownianParameter(m0, log(c0), mu, log(sigma)))
//   }

//   val dailyParams: Rand[Parameters] = for {
//       m0 <- Uniform(-2.0, 0.0)
//       c0 <- Uniform(0.5, 1.5)
//       t <- Uniform(0.3, 0.5)
//       a <- Uniform(0.0, 0.2)
//       s <- Uniform(0.1, 0.5)
//     } yield Parameters.leafParameter(None, SdeParameter.ouParameter(m0, log(c0), t, log(a), log(s)))

//   val weeklyParams: Rand[Parameters] = for {
//       m0 <- Uniform(0.0, 0.0)
//       c0 <- Uniform(0.5, 1.5)
//       t <- Uniform(0.1, 0.2)
//       a <- Uniform(0.01, 0.2)
//       s <- Uniform(0.1, 0.2)
//   } yield Parameters.leafParameter(None, SdeParameter.ouParameter(m0, log(c0), t, log(a), log(s)))

//   val params = for {
//     p1 <- negbinParams
//     p2 <- dailyParams
//     p3 <- weeklyParams
//   } yield p1 |+| p2 |+| p3

//   // build model from list of models
//   val models = List(
//     Model.negativeBinomial(Sde.brownianMotion(1)), 
//     Model.seasonalModel(24, 2, Sde.ouProcess(4)), 
//     Model.seasonalModel(24*7, 2, Sde.ouProcess(4)))
//   val model = models.reduce(_ |+| _)

//   val dataStream = DataFromFile("data/NegBin/SimulatedData.csv").
//     observations.
//     zip(Source(Stream.from(1))).
//     filter { case (d, i) => i % 10 == 0 }.
//     map(_._1)
// }

// object SimComposedNegBin extends App with NegBinomialTest with DataProtocols {
//   implicit val system = ActorSystem("NegBinTest")
//   implicit val materializer = ActorMaterializer()

//   // draw parameters from the prior and write them to a file
//   val p = params.draw
//   val pw = new PrintWriter("data/NegBin/ActualParameters.json")
//   pw.write(p.toJson.prettyPrint)
//   pw.close()

//   // simulate data using parameters drawn from the posterior
//   SimulateData(model(p)).
//     simRegular(5.0 / 60).
//     take(5000).
//     map(s => DecomposedModel(s.t, s.observation, s.eta, s.gamma, 
//       models.zipWithIndex.map { case (m, i) => SimulateData.getState(s.sdeState, m(p.getNode(i)), i)(s.t) })).
//     map(_.show).
//     runWith(Streaming.writeStreamToFile("data/NegBin/SimulatedData.csv")).
//     onComplete(_ => system.terminate())
// }

// object PilotRunComposedNegBin extends App with NegBinomialTest with DataProtocols {
//   implicit val system = ActorSystem("TrafficNegBin")
//   implicit val materializer = ActorMaterializer()

//   val particles = Vector(100, 200, 500, 1000, 2000, 5000)

//   val res = for {
//     data <- dataStream.runWith(Sink.seq)
//     vars = Streaming.pilotRun(data.toVector, model, params.draw, Resampling.systematicResampling, particles, 500)
//     io <- vars.map { case (n, v) => s"$n, $v" }.
//       runWith(Streaming.writeStreamToFile("data/NegBin/PilotRun.csv"))
//   } yield io

//   res.onComplete(_ => system.terminate())
// }

// object DetermineNegBinParameters extends App with NegBinomialTest with DataProtocols {
//   implicit val system = ActorSystem("TrafficNegBin")
//   implicit val materializer = ActorMaterializer()

//   def resample: Resample[State, Id] = Resampling.systematicResampling _

//   def initParams = scala.io.Source.fromFile("data/NegBin/ActualParameters.json").
//     getLines.
//     mkString.
//     parseJson.
//     convertTo[Parameters]

//   def chain(scale: Double, outfile: String, particles: Int) =
//     dataStream.
//       grouped(500).
//       mapConcat(d => (1 to 2).map(i => (d, i))).
//       mapAsync(2) { case (data, chain) => {
//         val filter = (p: Parameters) => ParticleFilter.filterLlState(data.toVector, resample, particles)(model(p))
//         Streaming.pmmhToJsonSerial(s"data/NegBin/$outfile-$particles-$scale-$chain.json", initParams, filter,
//           Parameters.perturb(scale), (p: Parameters) => 0.0, 1000000)
//       }}.
//       runWith(Sink.onComplete{ s =>
//         println(s)
//         system.terminate()
//       })

//   chain(0.05, "NegBinComposed", 150)
// }
