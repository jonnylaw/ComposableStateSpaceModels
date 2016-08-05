package model

import akka.stream.scaladsl.Source
import scala.concurrent.Future
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import java.io.File
import akka.stream.scaladsl._
import akka.util.ByteString
import model.Utilities._
import breeze.stats.{mean, variance}
import breeze.stats.distributions.Rand

object Streaming {

    /**
    * A class to monitor the state of ann MCMC chain
    * @param i the number of iterations computed
    * @param a the proportion of accepted moves
    */
  case class MonitorState(i: Int, a: Double)

  /**
    * A helper function to monitor the stream every 'every' iterations with a print statement
    * @param every number of iterations to wait until a print statement informs of the iteration, mll variance and acceptance ratio
    * @param chain, the chain number we are monitoring
    */
  def monitorStream(every: Int, chain: Int) = {
    Flow[MetropState].
      zip(Source(Stream.from(1))).
      grouped(every).
      map( x => {
        val iter = x map (_._2)
        MonitorState(iter.last, (x.map(_._1.accepted.toDouble).last)/iter.last)}
      ).
      map(m => println(s"""chain: $chain, iteration: ${m.i}, acceptance ratio: ${m.a}"""))
  }

  def runPmmhToFile(
    fileOut: String, chains: Int,
    initParams: Parameters, mll: Int => Parameters => LogLikelihood,
    perturb: Parameters => Rand[Parameters], particles: Int, iterations: Int): Unit = {

    implicit val system = ActorSystem("StreamingPmmh")
    implicit val materializer = ActorMaterializer()

    Source(1 to chains).
      mapAsync(parallelism = 4){ chain =>
        val iters = ParticleMetropolis(mll(particles), initParams, perturb).itersAkka

        println(s"""Running chain $chain, with $particles particles, $iterations iterations""")

        iters.
          zip(Source(Stream.from(1))).
          map{ case (x, i) => (i, x.params) }.
          take(iterations).
          map{ case (i, p) => ByteString(s"$i, $p\n") }.
          runWith(FileIO.toFile(new File(s"$fileOut-$iterations-$particles-$chain.csv")))
  
        iters.
          via(monitorStream(1000, chain)).
          runWith(Sink.ignore)
      }.
      runWith(Sink.onComplete { _ =>
        system.shutdown()
      })
  }

  /**
    * Asynchronously write a file
    */
  def writeFileSink[A](file: File) = {
    Flow[A].
      map(a => ByteString(a + "\n")).
      to(FileIO.toFile(file))
  }

  /**
    * Get parameter stream from file
    * @param
    */
  def readParameterStream(file: File) = {
    FileIO.fromFile(file).
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

  def cleanParameterFlow(file: File, burn: Int, thin: Int, totalIterations: Int) = {
    readParameterStream(file).
      via(burnin(burn)).
      via(thinParameters(thin)).
      via(calculateAverageOfParameters(totalIterations - burn)).
      toMat(Sink.head)(Keep.right)
  }
}
