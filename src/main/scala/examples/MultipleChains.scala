import akka.actor.ActorSystem
import akka.stream.ActorMaterializer

import model.Model._
import model.Filtering._
import model.POMP._
import model.StateSpace._
import model.DataTypes._
import model.State._
import model.Parameters._
import model.SimData._
import model.LeafParameter
import model.GaussianParameter
import model.BrownianParameter

object MultipleChains {
  def main(args: Array[String]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
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
    
    // define the particle filter, which calculates an empirical estimate of the marginal log-likelihood
    // this is a partially applied function, from Int => Parameters => LogLikelihood
    val mll = pfMll(data.toVector.sortBy(_.t), mod) _

    // specify the number of iterations for the MCMC
    // and number of particles for the particle filter
    val iterations = 10000
    val particles = 200

    // Write the iterations to files, the file will be named according to the prefix
    // "linearModel", the chain number, total iterations and particles
    // the method of proposing new parameters is gaussianPerturb, a random walk about the parameter space
    // with positive parameters proposed on a log scale
    runPmmhToFile("linearModel", 4, p, mll, gaussianPerturb(0.1, 0.1), particles, iterations)

  }
}
