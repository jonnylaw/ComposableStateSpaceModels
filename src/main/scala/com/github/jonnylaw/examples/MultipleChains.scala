package com.github.jonnylaw.examples

import com.github.jonnylaw.model._
import fs2._
import java.nio.file.Paths
import breeze.stats.distributions.{Gamma, Gaussian}
import breeze.linalg.{DenseVector, DenseMatrix}
import cats.Applicative
import cats.implicits._
import fs2.util.syntax._

object MultipleChains extends App {
  // provide a concurrency strategy
  implicit val S = fs2.Strategy.fromFixedDaemonPool(8, threadName = "worker")

  // define the model parameters, the initial state is Gaussian and the state space is generalised brownian motion
  val p = Parameters.leafParameter(
    Some(5.0),
    SdeParameter.brownianParameter(DenseVector(2.0), DenseMatrix((3.0)), 
      DenseVector(1.0), DenseMatrix((5.0)))
  )

  // the linear model has a Gaussian observation model, with V = 5.0 in this case
  val mod = Model.linearModel(Sde.brownianMotion)

  // simulate data from a simple linear model with five parameters, including measurement noise
  val times = (1 to 100).map(_.toDouble).toList
  val data = Stream.emits(times).
    through(SimulatedData(mod(p)).simPompModel(t0 = 1.0)).
    runLog.
    unsafeRun()

  // specify the number of particles in the particle filter
  val particles = 200
  
  // define the particle filter, which calculates an empirical estimate of the marginal log-likelihood
  // this is a partially applied function, from Int => Parameters => LogLikelihood
  val mll = ParticleFilter.likelihood(ParticleFilter.multinomialResampling, data, particles) compose mod

  // specify the number of iterations for the MCMC
  val iterations = 10000

  val prior: Parameters => LogLikelihood = p => { p match {
    case LeafParameter(Some(v), BrownianParameter(m0, c0, mu, sigma)) =>
      Gaussian(0.1, 10.0).logPdf(m0(0)) +
      Gamma(0.1, 10.0).logPdf(1/c0.data(0)) +
      Gamma(1.0, 1.0).logPdf(1/v) +
      Gaussian(-0.2, 10.0).logPdf(mu(0)) +
      Gamma(0.1, 10.0).logPdf(1/sigma.data(0))
  }}

  // Write the iterations to files, the file will be named according to the chain number
  // "linearModel", the chain number, total iterations and particles
  // the method of proposing new parameters is gaussianPerturb, a random walk about the parameter space
  // with positive parameters proposed on a log scale
  def iters(chain: Int): Task[Unit] = ParticleMetropolis(mll.run, p, Parameters.perturb(0.05), prior).
    iters[Task].
    take(10000).
    map((s: MetropState) => s.show).
    intersperse("\n").
    through(text.utf8Encode).
    through(io.file.writeAll(Paths.get(s"data/LinearModelPMMH-$chain.csv"))).
    run

  Task.parallelTraverse((1 to 4))(iters).
    unsafeRun
}
