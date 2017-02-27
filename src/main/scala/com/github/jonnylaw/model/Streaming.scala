package com.github.jonnylaw.model

import akka.stream.scaladsl._
import akka.stream._
import akka.util.ByteString
import breeze.stats.distributions.Rand
import cats._
import cats.data.Reader
import java.io._
import java.nio.file._
import scala.concurrent._
import scala.collection.parallel.immutable.ParVector
import scala.language.higherKinds

object Streaming {

  /**
    * Perform a pilot run 
    */
  def pilotRun[F[_]: Collection](
    data: Vector[Data], 
    model: Reader[Parameters, Model], 
    param: Parameters, 
    resample: Resample[F, Id, State],
    particles: Vector[Int])(implicit mat: Materializer, ec: ExecutionContext) = { 

    val proposal = (p: Parameters) => Rand.always(p)
    val prior = (p: Parameters) => 0.0

    def mll(n: Int) = ParticleFilter.likelihood[F](data, resample, n).compose(model)

    def iters(n: Int): Future[(Int, Double)] = {
      val its = ParticleMetropolis(mll(n).run, param, proposal, prior).
        params.
        take(100).
        runWith(Sink.seq)

      its map (s => (n, breeze.stats.variance(s.map(_.ll))))
    }

    Source.apply(particles).
      mapAsyncUnordered(4){ iters }
  }

  /**
    * An Akka Sink to write a stream of strings to a file
    * @param file the path of a file to write to
    */
  def writeStreamToFile(file: String): Sink[String, Future[IOResult]]  = {
    Flow[String].
      map(s => ByteString(s + "\n")).
      toMat(FileIO.toPath(Paths.get(file)))(Keep.right)
  }

  def serialize(value: Any): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(stream)
    oos.writeObject(value)
    oos.close()
    stream.toByteArray
  }

  def serialiseToFile(value: Any, file: String): Unit = {
    val bos = new BufferedOutputStream(new FileOutputStream(file, true))

    bos.write(serialize(value))
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
