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

object StepFilterBenchmark extends Bench.LocalTime with TrafficModel {
  val sizes: Gen[Int] = Gen.exponential("size")(100, 6400, 2)
  val filter = Filter(weeklyMod(simPrior.draw), Resampling.treeSystematicResampling)
  val input: Gen[PfState] = sizes map (n => filter.initialiseState(n, 0.0))

  performance of "Serial Particle Filter" in {
    using(input) in (init =>
      filter.stepFilter(init, TimedObservation(1.0, 1.0))
    )
  }

  val resample: Resample[State, Future] = Resampling.asyncTreeSystematicResampling(4) _

  val threads = Gen.enumeration("threads")(1, 2, 4)

  val asyncPf = FilterAsync(weeklyMod(simPrior.draw), resample)

  val initState = (n: Int) => asyncPf.initialiseState(n, 0.0)

  val parallelPfThreads = (threads: Int) => FilterAsync(weeklyMod(simPrior.draw), Resampling.asyncTreeSystematicResampling(threads) _)

  val parallelInput = for {
    s <- sizes
    t <- threads
  } yield (initState(s), t)

  performance of "Parallel Filter" in {    
    using(parallelInput) in { case (init, threads) =>
      Await.result(parallelPfThreads(threads).stepFilter(init, TimedObservation(1.0, 1.0)), Duration.Inf)
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
      filter.llFilter(data.toVector)(n)
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
