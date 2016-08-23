import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import com.github.jonnylaw.model._
import com.github.jonnylaw.model.Streaming._
import com.github.jonnylaw.model.POMP._
import com.github.jonnylaw.model.StateSpace._
import com.github.jonnylaw.model.Parameters._
import com.github.jonnylaw.model.SimData._
import com.github.jonnylaw.model.LeafParameter
import com.github.jonnylaw.model.GaussianParameter
import com.github.jonnylaw.model.BrownianParameter
import scala.concurrent.ExecutionContext.Implicits.global

object MultipleChains {
  def main(args: Array[String]) = {

    implicit val system = ActorSystem("StreamingPMMH")
    implicit val materializer = ActorMaterializer()

  // define the model parameters, the initial state is Gaussian and the state space is generalised brownian motion
    val p = LeafParameter(
      GaussianParameter(2.0, 3.0),
      Some(5.0),
      BrownianParameter(1.0, 5.0))

    // the linear model has a Gaussian observation model, with V = 5.0 in this case
    val mod = LinearModel(stepBrownian)

    // simulate data from a simple linear model with five parameters, including measurement noise
    val times = (1 to 100).map(_.toDouble).toList
    val data = simData(times, mod(p))

    // specify the number of particles in the particle filter
    val particles = 200
    
    // define the particle filter, which calculates an empirical estimate of the marginal log-likelihood
    // this is a partially applied function, from Int => Parameters => LogLikelihood
    val filter = Filter(mod, ParticleFilter.multinomialResampling, 1.0)
    val mll = filter.llFilter(data.toVector.sortBy(_.t))(particles) _

    // specify the number of iterations for the MCMC
    val iterations = 10000

    // Write the iterations to files, the file will be named according to the prefix
    // "linearModel", the chain number, total iterations and particles
    // the method of proposing new parameters is gaussianPerturb, a random walk about the parameter space
    // with positive parameters proposed on a log scale
    runPmmhToFile("linearModel", 4, p, mll, perturb(0.1), iterations)
  }
}
