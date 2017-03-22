package com.github.jonnylaw.model

import akka.stream.scaladsl._
import akka.stream._
import akka.NotUsed
import akka.util.ByteString
import breeze.stats.distributions.Rand
import breeze.linalg.DenseMatrix
import cats._
import cats.implicits._
import cats.data.Reader
import java.io._
import java.nio.file._
import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.parallel.immutable.ParVector
import scala.language.higherKinds
import spray.json._

object Streaming {

  /**
    * Perform a pilot run for the PMMH algorithm, to determine the number of particles to use in the filter
    */
  def pilotRun(
    data: Vector[Data], 
    model: Reader[Parameters, Model], 
    param: Parameters, 
    resample: Resample[State, Id],
    particles: Vector[Int])(implicit mat: Materializer, ec: ExecutionContext) = { 

    val proposal = (p: Parameters) => Rand.always(p)
    val prior = (p: Parameters) => 0.0

    def mll(n: Int) = ParticleFilter.likelihood(data, resample, n)(model(param))

    def iters(n: Int): Future[(Int, Double)] = {
      val lls = Source.repeat(1).
        map(i => mll(n)).
        zip(Source(Stream.from(1))).
        map { case (ll, i) => {
          ll
        }}.
        take(100).
        runWith(Sink.seq)

      lls map (ll => (n, breeze.stats.variance(ll)))
    }

    Source.apply(particles).
      mapAsyncUnordered(4){ iters }
  }


  def pmmhToJson(file: String, initParams: Parameters, filter: Parameters => Future[(LogLikelihood, Vector[StateSpace])],
    proposal: Parameters => Rand[Parameters], prior: Parameters => LogLikelihood, iters: Int)
    (implicit ec: ExecutionContext, mat: Materializer, f: JsonFormat[MetropState]): Future[IOResult] = {

    ParticleMetropolisStateAsync(filter, initParams, proposal, prior).
      iters.
      via(Streaming.monitorStateStream).
      take(iters).
      map(_.toJson.compactPrint).
      runWith(Streaming.writeStreamToFile(file))
  }
   

  /**
    * Given output from the PMMH algorithm, monitor the acceptance rate online
    */
  def monitorStream: Flow[ParamsState, ParamsState, NotUsed] = {
    Flow[ParamsState].
      zip(Source(Stream.from(1))).
      map { case (s, i) => {
        if (i % 100 == 0 ) {
          println(s"Iteration: $i, accepted: ${s.accepted.toDouble / i}")
          s
        } else {
          s
        }}}
  }

  def monitorStateStream: Flow[MetropState, MetropState, NotUsed] = {
    Flow[MetropState].
      zip(Source(Stream.from(1))).
      map { case (s, i) => {
        if (i % 100 == 0 ) {
          println(s"Iteration: $i, accepted: ${s.accepted.toDouble / i}")
          s
        } else {
          s
        }}}
  }

  /**
    * Given a stream, drop all but every nth element
    * @param n the thinning parameter, n = 5 will keep every 5th element
    */
  def thinStream[A](n: Int): Flow[A, A, NotUsed] = {
    Flow[A].
      zip(Source(Stream.from(1))).
      filter { case (_, i) => i % n == 0 }.
      map(_._1)
  }

  /**
    * Read from a JSON file containing the output from a PMMH run
    * @param file a string pointing to the JSON file to read
    * @param burnIn the number of iterations to drop from the front of the PMMH run
    * @param thin keep every thin'th iteration, specify thin = 1 for no thinning
    */
  def readPosterior(file: String, burnIn: Int, thin: Int)
    (implicit f: JsonFormat[MetropState]): Source[MetropState, Future[IOResult]] = {
    FileIO.fromPath(Paths.get(file)).
      via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
      map(_.utf8String).
      via(thinStream(thin)).
      drop(burnIn). // discard burn in iterations
      map(_.parseJson.convertTo[MetropState])
  }

  def readParamPosterior(file: String, burnIn: Int, thin: Int)
    (implicit f: JsonFormat[ParamsState]): Source[ParamsState, Future[IOResult]] = {
    FileIO.fromPath(Paths.get(file)).
      via(Framing.delimiter(ByteString("\n"), maximumFrameLength = 8192, allowTruncation = true)).
      map(_.utf8String).
      via(thinStream(thin)).
      drop(burnIn). // discard burn in iterations
      map(_.parseJson.convertTo[ParamsState])
  }

  /**
    * Create a distribution from a sequence, possibly utilising a transformation f
    * @param s a sequence of draws from a distribution, possibly from an MCMC runexamples
    * @param f a function to transform the draws from the distribution
    * @return a monadic distribution, Rand, which can be sampled from
    */
  def createDist[A, B](s: Seq[A])(f: A => B): Rand[B] = new Rand[B] {
    def draw = {
      f(Resampling.sampleOne(s.toVector))
    }
  }

  /**
    * An Akka Sink to write a stream of strings to a file
    * @param file the path of a file to write to
    */
  def writeStreamToFile(file: String): Sink[String, Future[IOResult]] = {
    Flow[String].
      map(s => ByteString(s + "\n")).
      toMat(FileIO.toPath(Paths.get(file)))(Keep.right)
  }

  def serialise(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  def serialiseToFile(value: Any, file: String)(
    implicit ec: ExecutionContext): Future[Unit] = Future {

    val bos = new BufferedOutputStream(new FileOutputStream(file, true))

    bos.write(serialise(value))
    bos.close()
  }

  def deserialise(bytes: Array[Byte]): Any = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    val value = ois.readObject
    ois.close
    value
  }

  def deserialiseFile(file: String): Any = {
    val fis = new FileInputStream(file)
    val ois = new Streaming.ObjectInputStreamWithCustomClassLoader(fis)

    val value = ois.readObject
    ois.close
    value
  }

  // magic: https://gist.github.com/ramn/5566596
  class ObjectInputStreamWithCustomClassLoader(
    fileInputStream: FileInputStream
  ) extends ObjectInputStream(fileInputStream) {
    override def resolveClass(desc: java.io.ObjectStreamClass): Class[_] = {
      try { Class.forName(desc.getName, false, getClass.getClassLoader) }
      catch { case ex: ClassNotFoundException => super.resolveClass(desc) }
    }
  }
}
