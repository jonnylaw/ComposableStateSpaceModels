package com.github.jonnylaw.model

import akka.stream.ClosedShape
import akka.stream.scaladsl.Source
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.nio.file.{Path, Paths}
import akka.stream.scaladsl._
import GraphDSL.Implicits._
import akka.util.ByteString
import com.github.jonnylaw.model.Utilities._
import breeze.stats.{mean, variance}
import breeze.stats.distributions.Rand

object Streaming {

    /**
    * A class to monitor the state of ann MCMC chain
    * @param i the number of iterations computed
    * @param a the proportion of accepted moves
    */
  case class MonitorState(i: Int, a: Double, mllVar: Option[Double])

  /**
    * A helper function to monitor the stream every 'every' iterations with a print statement
    * @param every number of iterations to wait until a print statement informs of the iteration, and acceptance ratio
    * @param chain, the chain number we are monitoring
    */
  def monitorStream(every: Int, chain: Int) = {
    Flow[MetropState].
      zip(Source(Stream.from(1))).
      grouped(every).
      map( x => {
        val iter = x map (_._2)
        MonitorState(
          iter.last,
          (x.map(_._1.accepted.toDouble).last)/iter.last,
          None)}
      ).
      map(m => println(s"""chain: $chain, iteration: ${m.i}, acceptance ratio: ${m.a}"""))
  }

  /**
    * Akka streaming iterations of Metropolis Hastings algorithm with asynchronous file writing
    * @param mh a metropolis hastings class
    * @param file a string representing the output filename
    * @param iters total amount of iterations to write to file
    * @return Unit, writes to a file asynchronously as 
    */
  def runMCMC(mh: MetropolisHastings, file: String, iters: Int) = {
    RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>
      val out = FileIO.toPath(Paths.get(s"./$file.csv"))
      val bcast = builder.add(Broadcast[MetropState](2))
      
      mh.itersAkka ~> bcast
      bcast ~> monitorStream(1000, 1) ~> Sink.ignore

      bcast ~> Flow[MetropState].take(iters) ~> Flow[MetropState].map(p => ByteString(s"$p\n")) ~> out

      ClosedShape
    })
  }

  /**
    * Used to monitor a pilot run of the PMMH algorithm
    */
  def monitorPilot(every: Int, chain: Int) = {
    Flow[MetropState].
      zip(Source(Stream.from(1))).
      grouped(every).
      map( x => {
        val iter = x map (_._2)
        val s = x map (_._1)
        MonitorState(
          iter.last,
          (s.map(_.accepted.toDouble).last)/iter.last,
          Some(breeze.stats.variance(s map (_.ll)))
        )
        }
      ).
      map(m => println(s"""chain: $chain, iteration: ${m.i}, 
                            mll variance: ${m.mllVar.get}, acceptance ratio: ${m.a}"""))
  }

  /**
    * Performs a pilot run of the PMMH algorithm, this runs PMMH iterations at a fixed parameter
    * value, returns the acceptance ratio and mll variance, the mll variance should be 1
    * Change the number of particles in the filter to alter the mll variance
    */
  def pilotRun(mll: Parameters => LogLikelihood, initParams: Parameters, iters: Int) = {
    val mh = ParticleMetropolis(mll, initParams, Parameters.proposeIdent)
    RunnableGraph.fromGraph(GraphDSL.create() { implicit builder =>
      val out = Sink.ignore
      val bcast = builder.add(Broadcast[MetropState](2))
      
      mh.itersAkka ~> bcast
      bcast ~> monitorPilot(iters/10, 1) ~> Sink.ignore

      bcast ~> Flow[MetropState].take(iters) ~> Flow[MetropState].map(p => ByteString(s"$p\n")) ~> out

      ClosedShape
    })
  }


  def runPmmhToFile(
    fileOut: String, chains: Int,
    initParams: Parameters, mll: Parameters => LogLikelihood,
    perturb: Parameters => Rand[Parameters], iterations: Int): Unit = {

    implicit val system = ActorSystem("StreamingPmmh")
    implicit val materializer = ActorMaterializer()

    Source(1 to chains).
      mapAsync(parallelism = 4){ chain =>
        val iters = ParticleMetropolis(mll, initParams, perturb).itersAkka

        println(s"""Running chain $chain, $iterations iterations""")

        iters.
          zip(Source(Stream.from(1))).
          map{ case (x, i) => (i, x.params) }.
          take(iterations).
          map{ case (i, p) => ByteString(s"$i, $p\n") }.
          runWith(FileIO.toPath(Paths.get(s"$fileOut-$chain.csv")))
  
        iters.
          via(monitorStream(1000, chain)).
          runWith(Sink.ignore)
      }.
      runWith(Sink.onComplete { _ =>
        system.terminate
      })
  }

  /**
    * Asynchronously write a file
    */
  def writeFileSink[A](file: Path) = {
    Flow[A].
      map(a => ByteString(a + "\n")).
      to(FileIO.toPath(file))
  }

  /**
    * Get parameter stream from file
    * @param file a filepath to read from
    */
  def readParameterStream(file: Path) = {
    FileIO.fromPath(file).
      via(Framing.delimiter(ByteString(System.lineSeparator), maximumFrameLength = 7862, allowTruncation = true)).
      map(_.utf8String).
      map(l => l.split(",")).
      map(r => r map (_.toDouble))
  }

  /**
    * Only keep every nth iteration, corresponding to the thin
    * @param thin keep every parameters where iteration % thin == 0
    * @return a thinned stream of Parameter Arrays
    */
  def thinParameters[A](thin: Int) = {
    Flow[A].zip(Source(Stream.from(1))).
      filter{ case (_, iteration) => iteration % thin == 0}.
      map{ case (p, _) => p }
  }

  /**
    * Remove burnin terms from a stream of parameters
    */
  def burnin[A](burn: Int) = {
    Flow[A].drop(burn)
  }

  /**
    * Calculate the average of a stream of parameters by grouping them and performing the operations on the grouped parameters
    */
  def calculateAverageOfParameters(totalIterations: Int) = {
    Flow[Array[Double]].grouped(totalIterations).
      map(params => params.transpose map (p => mean(p)))
  }

  def cleanParameterFlow(file: Path, burn: Int, thin: Int, totalIterations: Int) = {
    readParameterStream(file).
      via(burnin(burn)).
      via(thinParameters(thin)).
      via(calculateAverageOfParameters(totalIterations - burn)).
      toMat(Sink.head)(Keep.right)
  }
}
