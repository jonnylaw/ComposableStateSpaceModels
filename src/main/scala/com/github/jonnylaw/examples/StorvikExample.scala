package com.github.jonnylaw.examples

import com.github.jonnylaw.model._
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.stream.scaladsl._
import akka.util.ByteString
import scala.concurrent.ExecutionContext.Implicits.global
import java.nio.file.Paths
import breeze.stats.distributions._

object StorvikFilter extends App with LinearModel {
  implicit val system = ActorSystem("Storvik")
  implicit val materializer = ActorMaterializer()

  // read the data in as a Stream
  val data =  FileIO.fromPath(Paths.get("LinearModelSims.csv")).
    via(Framing.delimiter(
      ByteString("\n"),
      maximumFrameLength = 256,
      allowTruncation = true)).
    map(_.utf8String).
    map(a => a.split(",")).
    map(d => Data(d(0).toDouble, d(1).toDouble, None, None, None))

  // setup the Storvik Particle Filter

  // Proposal for the unknown observation variance
  def proposal = (s: SufficientStatistics) => s match {
    case x: GammaStatistics =>
      for {
        precision <- x.dist
      } yield LeafParameter(GaussianParameter(0.1, 1.0), Some(1.0/precision), BrownianParameter(-0.2, 1.0))
    case _ => throw new Exception("No idea lol")
  }

  val pf = StorvikGamma(unparamMod, proposal, 10000, Gamma(0.25, 4.0))

  data.
    via(pf.filter(p)).
    map(s => ByteString(s + "\n")).
    runWith(FileIO.toPath(Paths.get("StorvikGaussianUnknownPrecision.csv"))).
    onComplete(_ => system.terminate)
}
