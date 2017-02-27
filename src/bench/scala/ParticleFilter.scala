import org.scalameter.api._
import com.github.jonnylaw.model._
import cats._
import org.scalameter.picklers.Implicits._
import scala.concurrent.ExecutionContext.Implicits.global
import cats.implicits._
import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future
import breeze.linalg.DenseVector
import com.github.jonnylaw.examples._

object FilterBenchmark extends Bench.LocalTime with TestModel {
  val sizes = Gen.exponential("size")(100, 20000, 2)

  val filter = Filter[Vector, Id](
    model(params),
    Resampling.treeSystematicResampling)

  val input = sizes map (n => filter.initialiseState(n, 0.0))

  performance of "Serial Particle Filter" in {
    using(input) in (init =>
      filter.stepFilter(init, TimedObservation(1.0, 1.0))
    )
  }

  val asyncPf = Filter[Vector, Future](
    model(params),
    Resampling.asyncTreeSystematicResampling(4) _)

  val asyncInput = sizes map (n => asyncPf.initialiseState(n, 0.0))

  performance of "Serial with async resampling Particlefilter" in {
    using(asyncInput) in ( init =>
      asyncPf.stepFilter(init, TimedObservation(1.0, 1.0)).value
    )
  }

  val parallelPf = FilterAsync(
    4,
    model(params),
    Resampling.asyncTreeSystematicResampling(4) _)

  val parallelInput = sizes map (n => asyncPf.initialiseState(n, 0.0))

  performance of "Parallel Filter" in {    
    using(parallelInput) in ( init =>
      parallelPf.stepFilter(init, TimedObservation(1.0, 1.0)).value
    )
  }

}
