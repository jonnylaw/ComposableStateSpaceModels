package examples

import model._
import model.POMP._
import model.StateSpace._
import model.SimData._
import model.Parameters._
import model.ParticleFilter._

import breeze.stats.distributions.Rand
import akka.stream.scaladsl.Source
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

object MonadExample extends App {
  implicit val system = ActorSystem("MonadsEverywhere")
  implicit val materializer = ActorMaterializer()

  val times = (1 to 100).map(_.toDouble).toList
  val p = LeafParameter(GaussianParameter(0.0, 1.0), Some(1.0), BrownianParameter(1.0, 2.0))
  val mod = LinearModel(stepBrownian)

  val result: Rand[Source[Parameters, Any]] = for {
    data <- simDataRand(times, mod(p))
    mll = Filter(mod, multinomialResampling, 1.0).llFilter(data)(100) _
    s: Source[MetropState, Any] = ParticleMetropolisRand(mll, p, Parameters.perturb(0.05)).iters
    params = s map (_.params)
  } yield params

  result.draw take 1000 runForeach println
}
