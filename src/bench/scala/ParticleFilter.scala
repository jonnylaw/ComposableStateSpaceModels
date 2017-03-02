import akka.stream.scaladsl._
import akka.stream._
import akka.actor.ActorSystem

import org.scalameter.api._
import com.github.jonnylaw.model._
import cats._
import org.scalameter.picklers.Implicits._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import scala.collection.parallel.immutable.ParVector
import scala.concurrent._
import scala.concurrent.duration._
import breeze.linalg.DenseVector
import com.github.jonnylaw.examples._

object StepFilterBenchmark extends Bench.LocalTime with TestModel {
  val sizes = Gen.exponential("size")(100, 20000, 2)
  val filter = Filter(model(params), Resampling.treeSystematicResampling)
  val input = sizes map (n => filter.initialiseState(n, 0.0))

  performance of "Serial Particle Filter" in {
    using(input) in (init =>
      filter.stepFilter(init, TimedObservation(1.0, 1.0))
    )
  }

  val asyncPf = Filter(model(params), Resampling.asyncTreeSystematicResampling(4) _)

  val asyncInput = sizes map (n => asyncPf.initialiseState(n, 0.0))

  performance of "Serial with async resampling Particlefilter" in {
    using(asyncInput) in ( init =>
      Await.result(asyncPf.stepFilter(init, TimedObservation(1.0, 1.0)), Duration.Inf)
    )
  }

  val threads = Gen.enumeration("threads")(1, 2, 4)

  val parallelPf = (n: Int) => FilterAsync(model(params), Resampling.asyncTreeSystematicResampling(n) _)

  val parallelInput = for {
    s <- sizes
    t <- threads
  } yield (asyncPf.initialiseState(s, 0.0), t)

  performance of "Parallel Filter" in {    
    using(parallelInput) in { case (init, threads) =>
      Await.result(parallelPf(threads).stepFilter(init, TimedObservation(1.0, 1.0)), Duration.Inf)
    }
  }
}

/**
  * Run the entire particle filter using different methods
  */
object FilterBenchmark extends Bench.LocalTime with TestModel {
  val data: Seq[Data] = Await.result(SimulateData(model(params)).observations.take(500).runWith(Sink.seq), 10.seconds)

  val sizes = Gen.exponential("size")(100, 10000, 2)
  val filter = Filter(model(params), Resampling.treeSystematicResampling)

  performance of "Sequential Filter, systematic resampling" in {
    using (sizes) in { n =>
      Await.result(filter.llFilter(data.toVector)(n), Duration.Inf)
    }
  }
}

object AsyncFilterBenchMark extends Bench.LocalTime with TestModel {
  val data: Seq[Data] = Await.result(SimulateData(model(params)).observations.take(500).runWith(Sink.seq), 10.seconds)

  val input = for {
    s <- Gen.exponential("size")(100, 10000, 2)
    threads <- Gen.enumeration("threads")(1, 2, 4)
  } yield (s, threads)

  val asyncFilter = (n: Int) => FilterAsync(model(params), Resampling.asyncTreeSystematicResampling(n))

  performance of "Async Filter, async systematic resampling" in {
    using (input) in { case (n, threads) =>
      setParallelismGlobally(threads)
      Await.result(asyncFilter(threads).llFilter(data.toVector)(n), Duration.Inf)
    }
  }
}
