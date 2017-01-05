package com.github.jonnylaw.model

import scala.util.{Try, Success, Failure}
import breeze.stats.distributions.Rand._
import breeze.stats.distributions._
import breeze.numerics.log
import StateSpace._

import cats.implicits._
import cats.Monad
import cats.data.{Kleisli, EitherT}

import akka.stream.scaladsl.Source
import akka.NotUsed

case class MetropolisState[P](ll: LogLikelihood, p: P)

trait Metropolis[P] {
  def ll: P => LogLikelihood
  def proposal: P => Rand[P]
  def prior: ContinuousDistr[P]

  /**
    * A single step of a Metropolis algorithm, without re-evaluting the likelihood
    */
  def step_mh: MetropolisState[P] => Rand[MetropolisState[P]] =
    s => {

    for {
      prop_p <- proposal(s.p)
      prop_ll = ll(prop_p)
      a = prop_ll + prior.logPdf(prop_p) - s.ll - prior.logPdf(s.p)
      u <- Uniform(0, 1)
      next = if (log(u) < a) MetropolisState(prop_ll, prop_p) else s
    } yield next 
  }

  // I can lift this into a try
  // this is effectively a function from Try[MetropolisState[P]] => Try[Rand[MetropolisState[P]]]
  def try_step = Kleisli{ step_mh }.lift[Try]

  def iters(init_param: P): Process[MetropolisState[P]] = {
    val init_state = MetropolisState(-1e99, init_param)
    MarkovChain(init_state)(step_mh)
  }
}

/**
  * This function has error handling, which is a bit of a shame, because the error can't really 
  * happen here, I'm not sure how to pass a function which has a Try to a higher order function
  * without explicitly requiring the Try function to be passed inM
  */
trait MetropolisTry[P] {
  def ll: P => Try[LogLikelihood]
  def proposal: P => Rand[P]
  def prior: ContinuousDistr[P]
  def init_param: P

  /**
    * A single step of a Metropolis algorithm, without re-evaluting the likelihood
    * This is possibly the most terrible function ever
    * Can this be cleaned up with a Monad Transformer?
    */
  def step_mh: MetropolisState[P] => Try[Rand[MetropolisState[P]]] = ???
//    s => {
  //   val prop_p: Rand[P] = proposal(s.p)
  //   val prop_ll: Try[Rand[LogLikelihood]] = prop_p traverse (p => ll(p))
  //   for {
  //     ll <- prop_ll
  //     unif = Uniform(0, 1)
  //     a = prop_p.flatMap(p => ll map (l => l + prior.logPdf(p) - s.ll - prior.logPdf(s.p)))
  //     next = prop_p flatMap ((p: P) => a flatMap ((accept: Double) => ll flatMap (l => unif map ((u: Double) => if (log(u) < accept) MetropolisState(l, p) else s))))
  //   } yield next
  // }

  // Can we avoid calling get on this, this ruins everything :(
  def iters: Try[Process[MetropolisState[P]]] = {
    val init_state = MetropolisState(-1e99, init_param)

    Kleisli{ MarkovChain(init_state) }.lift[Try].run(s => step_mh(s).get)
  }

  def iters_stream: Try[Source[MetropolisState[P], NotUsed]] = {
    iters map(s => Source.fromIterator(() => s.steps))
  }


}

trait MetropolisMonad[P, M[_]] {
  implicit def monad: Monad[M]
}


/**
  * simplify the metropolis hastings with error handling using a monad transformer
  */
trait MetropolisTryTransformer[P] {
  def ll: P => Either[Throwable, LogLikelihood]
  def proposal: P => Rand[P]
  def prior: ContinuousDistr[P] = new ContinuousDistr[P] {
    def draw = ???
    def unnormalizedLogPdf(x: P): Double = 0.0
    def logNormalizer: Double = 0.0
  }
  def init_param: P

  // cats has support for a Monad Transformer on Either
  // Try is an Either with the result, A, on the right being a Success value
  // and the left being a Throwable

  /**
    * A single step of a Metropolis algorithm, which can return an error
    */
  def step_mh: MetropolisState[P] => EitherT[Rand, Throwable, MetropolisState[P]] = s => {
    for {
      prop_p <- EitherT.right(proposal(s.p))
      prop_ll <- EitherT.fromEither[Rand](ll(prop_p))
      a = prop_ll + prior.logPdf(prop_p) - s.ll - prior.logPdf(s.p)
      u <- EitherT.right(Uniform(0, 1): Rand[Double])
      next = if (log(u) < a) MetropolisState(prop_ll, prop_p) else s
    } yield next
  }

  /**
    * Multiple steps of the metropolis algorithm
    * What even is this?
    * Shouldn't this be an Either of something
    */
  def iters: Source[EitherT[Rand, Throwable, MetropolisState[P]], NotUsed] = {
    val init = MetropolisState(-1e99, init_param)
    Source.unfold(step_mh(init))(s => Some((s flatMap step_mh, s)))
  }

  // in order for this to work, do we need a RandT monad transformer?
  // def markov_iters = {
  //   val init = MetropolisState(-1e99, init_param)
  //   MarkovChain(step_mh(init))(s => s flatMap step_mh)
  // }
}

case class MetropolisParams(
  ll: Parameters => Either[Throwable, LogLikelihood],
  proposal: Parameters => Rand[Parameters],
  init_param: Parameters) extends MetropolisTryTransformer[Parameters]
