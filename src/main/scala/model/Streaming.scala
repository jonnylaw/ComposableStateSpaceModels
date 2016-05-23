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
import breeze.stats.mean

object Streaming {

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
