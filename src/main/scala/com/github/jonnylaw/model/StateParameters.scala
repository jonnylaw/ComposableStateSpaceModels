// package com.github.jonnylaw.model

// import breeze.linalg.{DenseVector, DenseMatrix, diag}
// import breeze.stats.distributions.{Rand, Gaussian, MultivariateGaussian}
// import breeze.numerics.exp
// import cats._
// import cats.implicits._
// import scala.util.{Try, Success, Failure}

// sealed trait StateParameter {
//   def sum(that: StateParameter): Try[StateParameter]

//   def perturb(delta: Double): Rand[StateParameter]

//   def perturbIndep(delta: Array[Double]): Rand[StateParameter]
// }

// case class GaussianParameter(m0: Double, c0: Double) extends StateParameter {

//   def sum(that: StateParameter): Try[StateParameter] = that match {
//     case GaussianParameter(m1, c1) => 
//       Success(StateParameter.gaussianParameter(m0 + m1, c0 + c1))
//     case _ =>
//       Failure(throw new Exception(s"Can't add GaussianParameter to $that"))
//   }

//   def perturb(delta: Double): Rand[StateParameter] = for {
//     innov <- MultivariateGaussian(
//       DenseVector.zeros[Double](2),
//       diag(DenseVector.fill(2)(delta)))
//     m = m0 + innov(0)
//     c = c0 * exp(innov(1))
//   } yield StateParameter.gaussianParameter(m, c)

//   def perturbIndep(delta: Array[Double]): Rand[StateParameter] = for {
//     innov <- MultivariateGaussian(
//       DenseVector.zeros[Double](2),
//       diag(DenseVector(delta)))
//     m = m0 + innov(0)
//     c = c0 * exp(innov(1))
//   } yield StateParameter.gaussianParameter(m, c)
// }

// object StateParameter {
//   /**
//     * Smart constructor for GaussianParameter to help with type inference
//     * @param m0 the mean of a Gaussian distribution
//     * @param c0 the covariance matrix of a Gaussian distribution
//     * @return an initial state parameter; a StateParameter object
//     */
//   def gaussianParameter(m0: Double, c0: Double): StateParameter = {

//     GaussianParameter(m0, c0)
//   }
// }
