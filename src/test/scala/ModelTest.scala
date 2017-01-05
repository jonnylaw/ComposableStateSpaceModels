// import com.github.jonnylaw.model._
// import State._
// import StateSpace._

// import org.scalatest._
// import breeze.numerics.{exp, log, sin, cos}
// import breeze.stats.distributions._
// import breeze.linalg.DenseVector
// import breeze.stats.{mean, variance}
// import breeze.linalg.{DenseVector, diag}
// import cats.implicits._
// import cats.Monoid

// class ModelSuite extends FlatSpec with Matchers {
//   def LinearModelNoNoise(stepFun: StepFunction): UnparamModel = new UnparamModel {
//     def apply(p: Parameters): Model = p match {
//       case LeafParameter(init, None, sdeparam) => {
//         Right(new Model {

//           def observation = x => new Rand[Observation] {
//             def draw = x.head
//           }

//           def f(s: State, t: Time) = s.head

//           def x0 = new Rand[State] { def draw = LeafState(0.0) }

//           def stepFunction = stepFun(sdeparam)

//           def dataLikelihood = (x, y) => y
//         })
//       }
//       case _ => Left(throw new Exception("LinearModelNoNoise needs LeafParameter"))
//     }
//   }

//   def SeasonalModelWithoutNoise(period: Int, harmonics: Int, stepFun: StepFunction): UnparamModel =
//     new UnparamModel {
//       def apply(p: Parameters): Model = p match {
//         case LeafParameter(init, None, sdeparam) =>
//           Right(new Model {

//             def observation = x => new Rand[Observation] {
//               def draw = x.head
//             }

//             def buildF(harmonics: Int, t: Time): DenseVector[Double] = {
//               val frequency = 2 * math.Pi / period
//               DenseVector(((1 to harmonics) flatMap (a => Array(cos(frequency * a * t), sin(frequency * a * t)))).toArray)
//             }

//             def f(s: State, t: Time) = s match {
//               case LeafState(x) =>
//                 buildF(harmonics, t) dot DenseVector(x.toArray)
//             }

//             def x0 = new Rand[State] {
//               def draw = LeafState(DenseVector.fill(harmonics*2)(0.0))
//             }

//             def stepFunction = stepFun(sdeparam)

//             def dataLikelihood = (x, y) => y
//           })
//         case _ => Left(throw new Exception("SeasonalModelNoNoise needs LeafParameter"))
//       }   
//     }

//   def stepNull(p: SdeParameter): (State, TimeIncrement) => Rand[State] = {
//     (s, dt) => new Rand[State] { def draw = s }
//   }

//   "Adding a null model" should "result in the same initial state" in {
//     val linearModel = LinearModelNoNoise(stepConstant)
//     val nullModel = Monoid[UnparamModel].empty
//     val combinedModel = linearModel |+| nullModel
//     val p = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))

//     val x0 = linearModel(p).map(_.x0.draw

//     assert(x0 == combinedModel(p).map(_.x0.draw)
//   }

//   "Adding a null model" should "result in the same step" in {
//     val linearModel = LinearModelNoNoise(stepConstant)
//     val nullModel = Monoid[UnparamModel].empty
//     val combinedModel = linearModel |+| nullModel
//     val p = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val x0 = linearModel(p).map(_.x0.draw
//     val x1 = linearModel(p).map(_.stepFunction(x0, 1).draw
//     assert(x1 == combinedModel(p).map(_.stepFunction(x0, 1).draw)
//   }

//   "Adding a null model" should "result in the same observation" in {
//     val linearModel = LinearModelNoNoise(stepConstant)
//     val nullModel = Monoid[UnparamModel].empty
//     val combinedModel = linearModel |+| nullModel
//     val p =LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val x0 = linearModel(p).map(_.x0.draw
//     val x1 = linearModel(p).map(_.stepFunction(x0, 1).draw
//     val lambda1 = linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))
//     val y1 = linearModel(p).map(_.observation(lambda1).draw

//     assert(y1 == combinedModel(p).map(_.observation(combinedModel(p).map(_.link(combinedModel(p).map(_.f(x1, 1))).draw)
//   }

//   "Adding a null model" should "result in the same data likelihood" in {
//     val linearModel = LinearModelNoNoise(stepConstant)
//     val nullModel = Monoid[UnparamModel].empty
//     val combinedModel = linearModel |+| nullModel
//     val p = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val x0 = linearModel(p).map(_.x0.draw
//     val x1 = linearModel(p).map(_.stepFunction(x0, 1).draw
//     val y = linearModel(p).map(_.observation(linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))).draw
//     val eta = linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))
//     val datalik = linearModel(p).map(_.dataLikelihood(eta, y)
//     val eta1 = linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))

//     assert(datalik == combinedModel(p).map(_.dataLikelihood(eta1, y))
//   }

//   "Constant Step Function" should "Advance the state by a constant * dt" in {
//     val p = StepConstantParameter(1.0)
//     val x0 = LeafState(1.0)

//     assert(stepConstant(p).map(_(x0, 2).draw == LeafState(3.0))
//   }

//   "Add the null model twice" should "result in the same model" in {
//     val linearModel = LinearModelNoNoise(stepConstant)
//     val nullModel = Monoid[UnparamModel].empty
//     val combinedModel = linearModel |+| nullModel
//     val p = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val combinedModel3 = linearModel |+| nullModel |+| nullModel
//     val x0 = linearModel(p).map(_.x0.draw
//     assert(x0 == combinedModel3(p).map(_.x0.draw)

//     val x1 = linearModel(p).map(_.stepFunction(x0, 1).draw
//     assert(x1 == combinedModel3(p).map(_.stepFunction(x0, 1).draw)

//     val y = linearModel(p).map(_.observation(linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))).draw
//     val eta = linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))
//     val datalik = linearModel(p).map(_.dataLikelihood(eta, y)

//     val eta1 = linearModel(p).map(_.link(linearModel(p).map(_.f(x1, 1))

//     assert(datalik == combinedModel3(p).map(_.dataLikelihood(eta, y))
//   }

//   "Combine two models" should "work" in {
//     val p = BranchParameter(
//       LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0)),
//       LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(3.0)))
//     val twoLinear = LinearModelNoNoise(stepConstant) |+| LinearModelNoNoise(stepConstant)

//     val x0 = twoLinear(p).map(_.x0.draw
//     assert(x0 == BranchState(LeafState(0.0), LeafState(0.0)))

//     val x1 = twoLinear(p).map(_.stepFunction(x0, 1).draw
//     assert(x1 == BranchState(LeafState(1.0), LeafState(3.0)))

//     val y = twoLinear(p).map(_.observation(twoLinear(p).map(_.link(twoLinear(p).map(_.f(x1, 1))).draw
//     assert(y == 4.0)
//   }

//   "Combine three models" should "result in a state space of three combined states" in {
//     val p1: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val p2: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(2.0))
//     val p3: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(3.0))

//     val p = p1 |+| p2 |+| p3

//     val threeLinear = LinearModelNoNoise(stepNull) |+| LinearModelNoNoise(stepConstant) |+| LinearModelNoNoise(stepConstant)

//     val x0 = threeLinear(p).map(_.x0.draw
//     val s1: State = LeafState(0.0)
//     assert(x0 == (s1 |+| s1 |+| s1))
//   }

//   "Combine three Models" should "advance each state space seperately" in {
//     val p1: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val p2: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(2.0))
//     val p3: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(3.0))

//     val p = p1 |+| p2 |+| p3

//     val threeLinear = LinearModelNoNoise(stepNull) |+| LinearModelNoNoise(stepConstant) |+| LinearModelNoNoise(stepConstant)

//     val x0 = threeLinear(p).map(_.x0.draw
//     val x1 = threeLinear(p).map(_.stepFunction(x0, 1).draw
//     val s1: State = LeafState(0.0)
//     val s2: State = LeafState(2.0)
//     val s3: State = LeafState(3.0)
//     assert(x1 == (s1 |+| s2 |+| s3))
//   }

//   "Combine three models" should "return an observation which is the sum of the state space, plus measurement error" in {
//     val p1: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))
//     val p2: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(2.0))
//     val p3: Parameters = LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(3.0))

//     val p = p1 |+| p2 |+| p3

//     val threeLinear = LinearModelNoNoise(stepNull) |+| LinearModelNoNoise(stepConstant) |+| LinearModelNoNoise(stepConstant)

//     val x0 = threeLinear(p).map(_.x0.draw
//     val x1 = threeLinear(p).map(_.stepFunction(x0, 1).draw
//     val eta = threeLinear(p).map(_.link(threeLinear(p).map(_.f(x1, 1))
//     val y = threeLinear(p).map(_.observation(eta).draw

//     assert(Math.abs(y - 3.0) < 3) // This will be true 99% of the time, since we have V = 1.0 and v ~ N(0,1)
//   }

//   "Combine three models" should "be associative" in {
//     val p = BranchParameter(
//       BranchParameter(LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0)),
//         LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0))),
//         LeafParameter(GaussianParameter(0.0, 10.0), Some(1.0), StepConstantParameter(1.0)))
//     val threeLinear1 = LinearModelNoNoise(stepNull) |+| LinearModelNoNoise(stepConstant) |+| LinearModelNoNoise(stepConstant)

//     val x0 = threeLinear1(p).map(_.x0.draw
//     assert(x0 == BranchState(
//       BranchState(LeafState(0.0), LeafState(0.0)),
//       LeafState(0.0)))

//     val x1 = threeLinear1(p).map(_.stepFunction(x0, 1).draw
//     assert(x1 == BranchState(BranchState(LeafState(0.0), LeafState(1.0)), LeafState(1.0)))
//   }
// }

// class LongRunningModelSuite extends FlatSpec with Matchers {

//   def stepNull(p: SdeParameter): (State, TimeIncrement) => Rand[State] = {
//     (s, dt) => new Rand[State] { def draw = s }
//   }

//   val tolerance = 1E-1

//   "A linear model" should "produce normally distributed observations" in {
//     val unparamMod = LinearModel(stepNull)
//     val p = LeafParameter(GaussianParameter(0.0, 1.0), Some(3.0), StepConstantParameter(0.0))
//     val mod = unparamMod(p)

//     val data = simData((1 to 100000).map(_.toDouble), mod)

//     val observations = data map (_.observation)
//     val firstState = mod.f(data.head.sdeState.get, 1)

//     assert(math.abs(mean(observations) - firstState) < tolerance)
//     assert(math.abs(variance(observations) - 9.0) < tolerance)
//   }

//   "A poisson model" should "produce poisson distributed observations" in {
//     val unparamMod = PoissonModel(stepNull)
//     val p = LeafParameter(GaussianParameter(1.0, 10.0), None, StepConstantParameter(0.0))
//     val mod = unparamMod(p)

//     // the state is constant at the first generated value, hence the rate lambda is constant
//     val data = simData((1 to 1000000).map(_.toDouble), mod)

//     val observations = data map (_.observation)
//     val state = data.head.sdeState.get
//     val lambda = mod.link(mod.f(state, 1)).head
//     assert(math.abs(mean(observations) - lambda) < tolerance)
//     assert(math.abs(variance(observations) - lambda) < tolerance)
//   }

//   "Bernoulli model" should "produce bernoulli distributed observations" in {
//     val unparamMod = BernoulliModel(stepNull)
//     val params = LeafParameter(GaussianParameter(1.0, 10.0), None, StepConstantParameter(0.0))
//     val mod = unparamMod(params)

//     val data = simData((1 to 100000).map(_.toDouble), mod)

//     // The state will remain constant at the first value
//     // hence the value of p will remain constant
//     val observations = data map (_.observation)
//     val state = data.head.sdeState.get
//     val p = mod.link(mod.f(state, 1)).head
//     val n = observations.size

//     assert(math.abs(mean(observations) - p) < tolerance)
//     assert(math.abs(variance(observations) - p*(1-p)) < tolerance)
//   }
// }
