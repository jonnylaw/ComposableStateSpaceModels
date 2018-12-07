// package com.github.jonnylaw.model

// import breeze.numerics.{cos, sin, sqrt, exp, log}
// import breeze.stats.distributions.{Bernoulli, Poisson, Gaussian, Rand, MultivariateGaussian, StudentsT, NegativeBinomial, Density}
// import breeze.linalg.{DenseMatrix, DenseVector}
// import scala.language.implicitConversions
// import java.io.Serializable

// object POMP {
//   /**
//     * Generalised student t model
//     * @param stepFun the diffusion process solution to use for this model
//     * @return an unparameterised model of the Student-T model, which can be composed with other models
//     */
//   def studentTModel(
//     stepFun: StepFunction, df: Int): UnparamModel =
//     new UnparamModel {
//       def apply(p: Parameters) = {
//         new Model {

//           def observation = x => p match {
//             case LeafParameter(_,v,_) => v match {
//               case Some(scale) => new Rand[Observation] {
//                 def draw = StudentsT(df).draw*scale + x.head
//               }
//             }
//             case _ => throw new Exception("Incorrect parameters supplied to Student-t model observation, expected LeafParameter")
//           }

//           def f(s: State, t: Time) = s.head

//           def x0 = p match {
//             case LeafParameter(stateParam, _, _) =>
//               stateParam match {
//                 case GaussianParameter(m0, c0) =>
//                   MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//               }
//             case _ => throw new Exception("Incorrect parameters supplied to initial state distribution of student t model")
//           }

//           def stepFunction = (x, dt) => p match {
//             case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
//             case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
//           }

//           def dataLikelihood = (eta, y) => p match {
//             case LeafParameter(_, v, _) => v match {
//               case Some(scale) => 1/scale * StudentsT(df).logPdf((y - eta.head) / scale)
//               case None => throw new Exception("No Parameter supplied for Student T scale")
//             }
//           }
//         }
//       }
//     }

//   /**
//     * A seasonal model
//     * @param period the period of the seasonality
//     * @param harmonics the number of harmonics to use in the seasonal model
//     * @param stepFun a solution to a diffusion process representing the latent state
//     */
//   def SeasonalModel(
//     period: Int,
//     harmonics: Int,
//     stepFun: StepFunction): UnparamModel =
//     new UnparamModel {
//       def apply(p: Parameters) = {
//         new Model {

//           def observation = x => new Rand[Observation] {
//             def draw = {
//               p match {
//                 case LeafParameter(_,v,_) => v match {
//                   case Some(noisesd) => Gaussian(x.head, noisesd).draw
//                   case None => throw new Exception("No variance parameter for seasonal model likelihood / obsevation")
//                 }
//                 case _ => throw new Exception("Incorrect parameters supplied to seasonal model")
//               }
//             }
//           }

//           def buildF(harmonics: Int, t: Time): DenseVector[Double] = {
//             val frequency = 2 * math.Pi / period
//             DenseVector(((1 to harmonics) flatMap (a =>
//               Array(cos(frequency * a * t), sin(frequency * a * t)))).toArray)
//           }

//           def f(s: State, t: Time) = s match {
//             case LeafState(x) => buildF(harmonics, t) dot DenseVector(x.toArray)
//             case _ => throw new Exception("Incorrect parameters supplied to transformation function of seasonal model")
//           }

//           def x0 = p match {
//             case LeafParameter(stateParam, _, _) =>
//               stateParam match {
//                 case GaussianParameter(m0, c0) =>
//                   MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//               }
//             case _ => throw new Exception("Incorrect parameters supplied to initial state of seasonal model")
//           }

//           def stepFunction = (x, dt) => p match {
//             case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
//             case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
//           }

//           def dataLikelihood = (eta, y) => p match {
//             case LeafParameter(_, v, _) => v match {
//               case Some(noiseSd) => Gaussian(eta.head, noiseSd).logPdf(y)
//             }
//             case _ => throw new Exception("Incorrect parameters supplied to data likelihood of seasonal model")
//           }
//         }
//       }
//     }

//   /**
//     * A linear unparameterised model
//     * @param stepFun a solution to a diffusion process representing the evolution of the latent state
//     * @return an UnparamModel which can be composed with other models
//     */
//   def LinearModel(stepFun: StepFunction): UnparamModel =  new UnparamModel {
//     def apply(p: Parameters) = {
//       new Model {
//         def observation = x => new Rand[Observation] {
//           def draw = {
//             p match {
//               case LeafParameter(_,v,_) => v match {
//                 case Some(noisesd) => Gaussian(x.head, noisesd).draw
//                 case None => throw new Exception("No variance parameter for linear model observation")
//               }
//               case _ => throw new Exception("Incorrect parameters supplied to Linear Model observation function")
//             }
//           }
//         }

//         def f(s: State, t: Time) = s.head

//         def x0 = p match {
//           case LeafParameter(stateParam, _, _) =>
//             stateParam match {
//               case GaussianParameter(m0, c0) =>
//                 MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//               case _ => throw new Exception("Incorrect initial state parameters in linear model")
//             }
//           case _ => throw new Exception("Incorrect parameters supplied to initial state of linear model")
//         }

//         def stepFunction = (x, dt) => p match {
//           case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
//           case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
//         }

//         def dataLikelihood = (eta, y) => p match {
//           case LeafParameter(_, v, _) => v match {
//             case Some(noiseSd) => Gaussian(eta.head, noiseSd).logPdf(y)
//             case _ => throw new Exception("No variance parameter for linear model likelihood")
//           }
//           case _ => throw new Exception("Incorrect parameters supplied to linear model data likelihood")
//         }
//       }
//     }
//   }

//   /**
//     * The Poisson unparameterised model with a one dimensional latent state
//     * @param stepFun a solution to a diffusion process representing the evolution of the latent space
//     * @return a Poisson UnparamModel which can be composed with other UnparamModels
//     */
//   def PoissonModel(stepFun: StepFunction): UnparamModel = new UnparamModel {
//     def apply(p: Parameters) = {
//       new Model {

//         def observation = lambda => new Rand[Observation] {
//           def draw = Poisson(lambda.head).draw
//         }

//         override def link(x: Double) = Vector(exp(x))

//         def f(s: State, t: Time) = s.head

//         def x0 = p match {
//           case LeafParameter(stateParam, _, _  @unchecked) =>
//             stateParam match {
//               case GaussianParameter(m0, c0) =>
//                 MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//               case _ => throw new Exception("Incorrect initial state parameter in poisson model x0")
//             }
//           case _ => throw new Exception("Incorrect parameters supplied to poisson x0, needed LeafParameter")
//         }

//         def stepFunction = (x, dt) => p match {
//           case LeafParameter(_,_,sdeparam  @unchecked) => stepFun(sdeparam)(x, dt)
//           case _ => throw new Exception("Incorrect parameter to poisson step function, should receive a Leaf Parameter")
//         }

//         def dataLikelihood = (lambda, y) => Poisson(lambda.head).logProbabilityOf(y.toInt)
//       }
//     }
//   }

//   /**
//     * The bernoulli model with a one dimensional latent state
//     * @param stepFun a solution to a diffusion process 
//     */
//   def BernoulliModel(stepFun: StepFunction): UnparamModel = new UnparamModel {
//     def apply(params: Parameters) =
//       new Model {

//         def observation = p => new Rand[Observation] {
//           def draw = {
//             scala.util.Random.nextDouble < p.head
//           }
//         }

//         override def link(x: Gamma) = {
//           if (x > 6) {
//             Vector(1.0)
//           } else if (x < -6) {
//             Vector(0.0)
//           } else {
//             Vector(1.0/(1 + exp(-x)))
//           }
//         }

//         def f(s: State, t: Time) = s.head

//         def x0 = params match {
//           case LeafParameter(stateParam, _, _ @unchecked) =>
//             stateParam match {
//               case GaussianParameter(m0, c0) =>
//                 MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//             }
//         }

//         def stepFunction = (x, dt) => params match {
//           case LeafParameter(_,_,sdeparam @unchecked) => stepFun(sdeparam)(x, dt)
//           case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
//         }

//         def dataLikelihood = (p, y) => {
//           if (y) {
//             if (p.head == 0.0) -1e99 else log(p.head)
//           } else {
//             if (p.head == 1.0) -1e99 else log(1-p.head)
//           }
//         }

//       }
//   }

//   /**
//     * The Log-Gaussian Cox-Process is used to model time to event data with 
//     * log-gaussian varying hazard rate
//     */
//   def LogGaussianCox(stepFun: StepFunction): UnparamModel =
//     new UnparamModel {
//       def apply(p: Parameters) =
//         new Model {

//           def observation = s => new Rand[Observation] {
//             def draw: Observation = ???
//           }

//           def f(s: State, t: Time) = s.head

//           def x0 = p match {
//             case LeafParameter(stateParam, _, _ @unchecked) =>
//               stateParam match {
//                 case GaussianParameter(m0, c0) =>
//                   MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//               }
//           }

//           def stepFunction = (x, dt) => p match {
//             case LeafParameter(_,_,sdeparam @unchecked) => stepFun(sdeparam)(x, dt)
//             case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
//           }

//           def dataLikelihood = (lambda, y) => lambda.head - lambda(1)
//         }
//     }

//   def negativeBinomial(stepFun: StepFunction): UnparamModel =
//     new UnparamModel {
//       def apply(p: Parameters) = {
//         new Model {

//           def observation = mu => new Rand[Observation] {
//             def draw = {
//               p match {
//                 case LeafParameter(_, scale, _) =>
//                   val sigma = scale.get
//                   val p = (sigma*sigma - mu.head) / sigma*sigma
//                   val r = mu.head * mu.head / (sigma * sigma - mu.head)

//                   NegativeBinomial(r, p).draw
//               }
//             }
//           }

//           override def link(x: Double) = Vector(exp(x))

//           def f(s: State, t: Time) = s.head

//           def x0 = p match {
//             case LeafParameter(stateParam, _, _ @unchecked) =>
//               stateParam match {
//                 case GaussianParameter(m0, c0) =>
//                   MultivariateGaussian(m0, sqrt(c0)) map (LeafState(_))
//               }
//             case _ => throw new Exception("State of single model must receive a Leaf Parameter")
//           }

//           def stepFunction = (x, dt) => p match {
//             case LeafParameter(_,_,sdeparam @unchecked) => stepFun(sdeparam)(x, dt)
//             case _ => throw new Exception("Step Function from a single model should receive a Leaf Parameter")
//           }

//           def dataLikelihood = (mu, y) => p match {
//               case LeafParameter(_, scale, _) =>
//                 val sigma = scale.get
//                 val p = (sigma*sigma - mu.head) / sigma*sigma
//                 val r = mu.head * mu.head / (sigma * sigma - mu.head)

//                 NegativeBinomial(r, p).logProbabilityOf(y.toInt)
//               case _ => throw new Exception("Can't determine the likelihood using a branch parameter")
//             }

//         }
//       }
//     }
// }
