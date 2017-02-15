import com.github.jonnylaw.model._

import org.scalatest._
import breeze.numerics.{exp, log, sin, cos}
import breeze.stats.distributions._
import breeze.stats.{mean, variance}
import breeze.linalg.{DenseVector, diag, DenseMatrix}
import cats.implicits._
import cats.{Monoid, Applicative}
import cats.data.Reader
import scala.util.{Try, Failure, Success}
import fs2._

class ModelSuite extends FlatSpec with Matchers {
  // a simple linear model with no observation noise, for testing
  case class LinearModelNoNoise(sde: Sde, p: LeafParameter) extends Model {
    def observation = x => Rand.always(x)

    def f(s: State, t: Time) = s.fold(0.0)((x: DenseVector[Double]) => x(0))(_ + _)

    def dataLikelihood = (x, y) => y
  }
  // smart constructor for linear model
  def linearModelNoNoise(sde: SdeParameter => Sde): Reader[Parameters, Model] = Reader { p: Parameters => p match {
    case param: LeafParameter => LinearModelNoNoise(sde(param.sdeParam), param)
    case _ => throw new Exception
  }}

  case class SeasonalModelNoNoise(
    period: Int, harmonics: Int, sde: Sde, p: LeafParameter) extends Model {

      def observation = x => Rand.always(x)

      def buildF(harmonics: Int, t: Time): DenseVector[Double] = {
        val frequency = 2 * math.Pi / period
        val res = (1 to harmonics).
          toArray.
          flatMap(a => Array(cos(frequency * a * t), sin(frequency * a * t)))

        DenseVector(res)
      }

      def f(s: State, t: Time) = s.fold(0.0)(x => buildF(harmonics, t) dot x)(_ + _)

      def dataLikelihood = (x, y) => y
  }

  def seasonalModelNoNoise(
    period: Int,
    h: Int,
    sde: SdeParameter => Sde): Reader[Parameters, Model] = Reader { p: Parameters => p match {

    case param: LeafParameter => SeasonalModelNoNoise(period, h, sde(param.sdeParam), param)
    case _ => throw new Exception
  }}

  def stepNull(p: SdeParameter): Sde = new Sde {
    def initialState: Rand[State] = Rand.always(Tree.leaf(DenseVector(0.0)))
    def drift(state: State): Tree[DenseVector[Double]] = Tree.leaf(DenseVector(1.0))
    def diffusion(state: State) = Tree.leaf(DenseMatrix((0.0)))
    def dimension: Int = 1
    override def stepFunction(dt: TimeIncrement)(s: State) = Rand.always(s)
  }

  "Brownian Motion step function" should "Change the value of the state" in {
    val p = SdeParameter.brownianParameter(
      DenseVector(1.0), DenseVector(1.0),
      DenseVector(1.0), DenseVector(1.0))

    val x0 = Tree.leaf(DenseVector(1.0))

    assert(Sde.brownianMotion(p).stepFunction(2)(x0).draw.getNode(0) != x0.getNode(0))
  }

  "Compose two models" should "work" in {
    val singleP = Parameters.leafParameter(
      Some(1.0), SdeParameter.brownianParameter(
      DenseVector(1.0), DenseVector(1.0), DenseVector(1.0), DenseVector(1.0)))

    val p = singleP |+| singleP

    val twoLinear = linearModelNoNoise(Sde.brownianMotion) |+|
    linearModelNoNoise(Sde.brownianMotion)

    val x0 = twoLinear(p).sde.initialState.draw
    assert(x0.flatten.size == twoLinear(p).sde.dimension)

    val x1 = twoLinear(p).sde.stepFunction(1)(x0).draw
    assert(x1.getNode(0) != x0.getNode(1))

    val eta = twoLinear(p).link(twoLinear(p).f(x1, 1))
    val y = twoLinear(p).observation(eta).draw
    assert(y == eta)
  }

  "Combine three models" should "result in a state space of three combined states" in {
    val p = List.fill(3)(
      Parameters.leafParameter(Some(1.0), SdeParameter.brownianParameter(
      DenseVector(1.0), DenseVector(1.0), DenseVector(1.0), DenseVector(0.1)))).
      reduce((a, b) => a |+| b)

    val threeLinear = linearModelNoNoise(stepNull) |+|
      linearModelNoNoise(Sde.brownianMotion) |+|
      linearModelNoNoise(Sde.brownianMotion)

    val x0 = threeLinear(p).sde.initialState
    assert(x0.draw.flatten.size == 3)
  }

  "Combine three Models" should "advance each state space seperately" in {
    val p = List.fill(3)(
      Parameters.leafParameter(Some(1.0), SdeParameter.brownianParameter(
      DenseVector(1.0), DenseVector(1.0), DenseVector(1.0), DenseVector(0.1)))).
      reduce((a, b) => a |+| b)

    val threeLinear = linearModelNoNoise(Sde.brownianMotion) |+|
      linearModelNoNoise(Sde.brownianMotion) |+|
      linearModelNoNoise(Sde.brownianMotion)

    val x0 = threeLinear(p).sde.initialState.draw
    val x1 = threeLinear(p).sde.stepFunction(1)(x0).draw
    val s1 = Tree.leaf(DenseVector(0.0))
    val s2 = Tree.leaf(DenseVector(2.0))
    val s3 = Tree.leaf(DenseVector(3.0))

    assert(x1.getNode(0) != s1.getNode(0))
    assert(x1.getNode(1) != s2.getNode(0))
    assert(x1.getNode(2) != s3.getNode(0))
    assert(s1.getNode(0) != s2.getNode(0))
  }

  "Combine three models" should "return an observation which is the sum of the state space, plus measurement error" in {
    val p = List.fill(3)(
      Parameters.leafParameter(Some(1.0), SdeParameter.brownianParameter(
      DenseVector(1.0), DenseVector(1.0), DenseVector(1.0), DenseVector(0.1)))).
      reduce((a, b) => a |+| b)

    val threeLinear = linearModelNoNoise(stepNull) |+|
      linearModelNoNoise(Sde.brownianMotion) |+|
      linearModelNoNoise(Sde.brownianMotion)

    val x0 = threeLinear(p).sde.initialState.draw
    val x1 = threeLinear(p).sde.stepFunction(1)(x0).draw
    val eta = threeLinear(p).link(threeLinear(p).f(x1, 1))
    val y = threeLinear(p).observation(eta).draw

    assert(y == eta) 
  }
}

// class LongRunningModelSuite extends FlatSpec with Matchers {
//   def stepNull(p: SdeParameter): Sde = new Sde {
//     def initialState: Rand[State] = Rand.always(Tree.leaf(DenseVector(0.0))
//     def drift(state: State): Tree[DenseVector[Double]] = Tree.leaf(DenseVector(1.0)
//     def diffusion(state: State): Tree[DenseVector[Double]] = Tree.leaf(DenseVector(0.0))
//     def dimension: Int = 1
//     override def stepFunction(dt: TimeIncrement)(s: State) = Rand.always(s)
//   }
//   val tolerance = 1E-1

//   "A linear model" should "produce normally distributed observations" in {
//     val unparamMod = LinearModel(stepNull)
//     val p = LeafParameter(GaussianParameter(0.0, 1.0), Some(3.0), StepConstantParameter(0.0)
//     val mod = unparamMod(p)

//     val data = simData(1 to 100000).map(_.toDouble), mod)

//     val observations = data map (_.observation)
//     val firstState = mod.f(data.sdeState.get, 1)

//     assert(math.abs(mean(observations) - firstState) < tolerance)
//     assert(math.abs(variance(observations) - 9.0) < tolerance)
//   }

//   "A poisson model" should "produce poisson distributed observations" in {
//     val unparamMod = PoissonModel(stepNull)
//     val p = LeafParameter(GaussianParameter(1.0, 10.0), None, StepConstantParameter(0.0)
//     val mod = unparamMod(p)

//     // the state is constant at the first generated value, hence the rate lambda is constant
//     val data = simData(1 to 1000000).map(_.toDouble), mod)

//     val observations = data map (_.observation)
//     val state = data.sdeState.get
//     val lambda = mod.link(mod.f(state, 1)
//     assert(math.abs(mean(observations) - lambda) < tolerance)
//     assert(math.abs(variance(observations) - lambda) < tolerance)
//   }

//   "Bernoulli model" should "produce bernoulli distributed observations" in {
//     val unparamMod = BernoulliModel(stepNull)
//     val params = LeafParameter(GaussianParameter(1.0, 10.0), None, StepConstantParameter(0.0)
//     val mod = unparamMod(params)

//     val data = simData(1 to 100000).map(_.toDouble), mod)

//     // The state will remain constant at the first value
//     // hence the value of p will remain constant
//     val observations = data map (_.observation)
//     val state = data.sdeState.get
//     val p = mod.link(mod.f(state, 1)
//     val n = observations.size

//     assert(math.abs(mean(observations) - p) < tolerance)
//     assert(math.abs(variance(observations) - p*(1-p) < tolerance)
//   }
// }
