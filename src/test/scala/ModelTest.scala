import com.github.jonnylaw.model._
import org.scalatest._
import breeze.numerics.{sin, cos}
import breeze.stats.distributions._
import breeze.stats.{mean, variance}
import breeze.linalg.{DenseVector, diag, DenseMatrix}
import cats.data.ReaderT
import spire.implicits._
import scala.util.{Failure, Success}

class ModelSuite extends FlatSpec with Matchers {
  // a simple linear model with no observation noise, for testing
  case class LinearModelNoNoise(sde: Sde, p: ParamNode) extends Model {
    def observation = x => Rand.always(x)

    def f(s: State, t: Time) = s.fold(0.0)((x: DenseVector[Double]) => x(0))(_ + _)

    def dataLikelihood = (x, y) => y
  }

  // smart constructor for linear model
  def linearModelNoNoise(sde: UnparamSde): UnparamModel = ReaderT { p: Parameters => p match {
    case Leaf(param) => sde.run(param.sdeParam).map(s => LinearModelNoNoise(s, param))
    case _ => Failure(throw new Exception)
  }}

  case class SeasonalModelNoNoise(
    period: Int, harmonics: Int, sde: Sde, p: ParamNode) extends Model {

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
    sde: UnparamSde): UnparamModel = ReaderT { p: Parameters => p match {
    case Leaf(param) => sde.run(param.sdeParam).map(s => SeasonalModelNoNoise(period, h, s, param))
    case _ => Failure(throw new Exception)
  }}

  def stepNull: UnparamSde = ReaderT { (p: SdeParameter) =>
    Success(new Sde {
      def initialState: Rand[State] = Rand.always(Tree.leaf(DenseVector(0.0)))
      def drift(state: State): Tree[DenseVector[Double]] = Tree.leaf(DenseVector(1.0))
      def diffusion(state: State) = Tree.leaf(DenseMatrix((0.0)))
      def dimension: Int = 1
      override def stepFunction(dt: TimeIncrement)(s: State) = Rand.always(s)
    })
  }

  "Brownian Motion step function" should "Change the value of the state" in {
    val p = SdeParameter.brownianParameter(1.0)(1.0)(1.0)

    val x0 = Tree.leaf(DenseVector(1.0))

    assert(Sde.brownianMotion(1)(p).get.stepFunction(2)(x0).draw.getNode(0) != x0.getNode(0))
  }

  "Compose two models" should "work" in {
    val singleP = Tree.leaf(ParamNode(Some(1.0), SdeParameter.brownianParameter(1.0)(1.0)(1.0)))

    val p = singleP |+| singleP

    val unparamMod = linearModelNoNoise(Sde.brownianMotion(1))

    for {
      mod <- (unparamMod |+| unparamMod)(p)
      x0 = mod.sde.initialState.draw
      x1 = mod.sde.stepFunction(1)(x0).draw
      eta = mod.link(mod.f(x1, 1))
      y = mod.observation(eta).draw
    } yield assert(x0.flatten.size == mod.sde.dimension && x1.getNode(0) != x0.getNode(1) && y == eta)
  }

  // "Combine three models" should "result in a state space of three combined states" in {
  //   val p = List.fill(3)(
  //     Tree.leaf(ParamNode(Some(1.0), SdeParameter.brownianParameter(1.0)(1.0)(1.0)))).
  //     reduce((a, b) => a |+| b)

  //   val threeLinear = linearModelNoNoise(stepNull) |+|
  //     linearModelNoNoise(Sde.brownianMotion(1)) |+|
  //     linearModelNoNoise(Sde.brownianMotion(1))

  //   for {
  //     mod <- threeLinear(p)
  //     x0 = mod.sde.initialState
  //   } yield assert(x0.draw.flatten.size == 3)
  // }

  // "Combine three Models" should "advance each state space seperately" in {
  //   val p = List.fill(3)(
  //     Tree.leaf(ParamNode(Some(1.0), SdeParameter.brownianParameter(1.0)(1.0)(1.0)))).
  //     reduce((a, b) => a |+| b)

  //   val threeLinear = linearModelNoNoise(Sde.brownianMotion(1)) |+|
  //     linearModelNoNoise(Sde.brownianMotion(1)) |+|
  //     linearModelNoNoise(Sde.brownianMotion(1))

  //   for {
  //     mod <- threeLinear(p)
  //     x0 = mod.sde.initialState.draw
  //     x1 = mod.sde.stepFunction(1)(x0).draw
  //     s1 = Tree.leaf(DenseVector(0.0))
  //     s2 = Tree.leaf(DenseVector(2.0))
  //     s3 = Tree.leaf(DenseVector(3.0))
  //   } yield assert(x1.getNode(0) != s1.getNode(0) && x1.getNode(1) != s2.getNode(0) && x1.getNode(2) != s3.getNode(0) && s1.getNode(0) != s2.getNode(0))
  // }

  // "Combine three models" should "return an observation which is the sum of the state space, plus measurement error" in {
  //   val p = List.fill(3)(
  //     Tree.leaf(ParamNode(Some(1.0), SdeParameter.brownianParameter(1.0)(1.0)(1.0)))).
  //     reduce((a, b) => a |+| b)

  //   val threeLinear = linearModelNoNoise(stepNull) |+|
  //     linearModelNoNoise(Sde.brownianMotion(1)) |+|
  //     linearModelNoNoise(Sde.brownianMotion(1))

  //   for {
  //     mod <- threeLinear(p)
  //     x0 = mod.sde.initialState.draw
  //     x1 = mod.sde.stepFunction(1)(x0).draw
  //     eta = mod.link(mod.f(x1, 1))
  //     y = mod.observation(eta).draw
  //   } yield assert(y == eta) 
  // }
}
