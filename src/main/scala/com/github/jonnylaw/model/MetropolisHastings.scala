// package com.github.jonnylaw.model

// import scala.util.{Try, Success, Failure}
// import breeze.stats.distributions.Rand._
// import breeze.stats.distributions._
// import breeze.numerics.log
// import StateSpace._

// import cats.implicits._
// import cats.data.Kleisli

// import akka.stream.scaladsl.Source
// import akka.NotUsed

// case class MetropState[P](ll: LogLikelihood, p: P)

// trait Metropolis[P] {
//   def ll: P => LogLikelihood
//   def proposal: P => Rand[P]
//   def prior: ContinuousDistr[P]

//   /**
//     * A single step of a Metropolis algorithm, without re-evaluting the likelihood
//     */
//   def step_mh: MetropState[P] => Rand[MetropState[P]] =
//     s => {

//     for {
//       prop_p <- proposal(s.p)
//       prop_ll = ll(prop_p)
//       a = prop_ll + prior.logPdf(prop_p) - s.ll - prior.logPdf(s.p)
//       u <- Uniform(0, 1)
//       next = if (log(u) < a) MetropState(prop_ll, prop_p) else s
//     } yield next 
//   }

//   // I can lift this into a try
//   // this is effectively a function from Try[MetropState[P]] => Try[Rand[MetropState[P]]]
//   def try_step = Kleisli{ step_mh }.lift[Try]

//   def iters(init_param: P): Process[MetropState[P]] = {
//     val init_state = MetropState(-1e99, init_param)
//     MarkovChain(init_state)(step_mh)
//   }
// }

// /**
//   * This function has error handling, which is a bit of a shame, because the error can't really 
//   * happen here, I'm not sure how to pass a function which has a Try to another function
//   */
// trait MetropolisTry[P] {
//   def ll: P => Try[LogLikelihood]
//   def proposal: P => Rand[P]
//   def prior: ContinuousDistr[P]
//   def init_param: P

//   /**
//     * A single step of a Metropolis algorithm, without re-evaluting the likelihood
//     * This is possibly the most terrible function ever
//     * Can this be cleaned up with a Monad Transformer?
//     */
//   def step_mh: MetropState[P] => Try[Rand[MetropState[P]]] = s => {
//     val prop_p: Rand[P] = proposal(s.p)
//     val prop_ll: Try[Rand[LogLikelihood]] = prop_p traverse (p => ll(p))
//     for {
//       ll <- prop_ll
//       unif = Uniform(0, 1)
//       a = prop_p.flatMap(p => ll map (l => l + prior.logPdf(p) - s.ll - prior.logPdf(s.p)))
//       next = prop_p flatMap ((p: P) => a flatMap ((accept: Double) => ll flatMap (l => unif map ((u: Double) => if (log(u) < accept) MetropState(l, p) else s))))
//     } yield next
//   }

//   // Can we avoid calling get on this, I think this ruins everything :(
//   def iters: Try[Process[MetropState[P]]] = {
//     val init_state = MetropState(-1e99, init_param)

//     Kleisli{ MarkovChain(init_state) }.lift[Try].run(s => step_mh(s).get)
//   }

//   def iters_stream: Try[Source[MetropState[P], NotUsed]] = {
//     iters map(s => Source.fromIterator(() => s.steps))
//   }
// }

// case class MetropolisParams(
//   ll: Parameters => Try[LogLikelihood],
//   proposal: Parameters => Rand[Parameters],
//   init_param: Parameters,
//   prior: ContinuousDistr[Parameters]) extends MetropolisTry[Parameters]

// /**
//   * simplify the metropolis hastings with error handling using a monad transformer
//   */
// trait MetropolisTryTransformer[P] {
//   def ll: P => Try[LogLikelihood]
//   def proposal: P => Rand[P]
//   def prior: ContinuousDistr[P]

//   // cats has support for a Monad Transformer on Either
//   // Try is an Either with the result, A, on the right being a Success value
//   // and the left being a Throwable
//   import cats.data.EitherT
//   type TryT[F[_], A] = EitherT[F, Throwable, A]

//   /**
//     * A single step of a Metropolis algorithm, without re-evaluting the likelihood
//     * This is possibly the most terrible function ever
//     */
//   def step_mh: MetropState[P] => TryT[Rand, MetropState[P]] = ???

//   // def iters(init_param: P): Process[MetropState[P]] = {
//   //   val init_state = MetropState(-1e99, init_param)
//   //   MarkovChain(init_state)(step_mh)
//   // }
// }

// object MetropolisHastings {
//   // there is a wrapper for a function of type A => F[B] called Kliesli
//   import cats.data.Kleisli

//   // define some parameters
//   def p: Parameters = LeafParameter(
//     GaussianParameter(0.0, 1.0),
//     Some(1.0),
//     BrownianParameter(0.3, 1.0))

//   // create a model
//   def mod: Try[Model] = LinearModel(stepBrownian)(p)

//   // simulate some data from the model
//   def data: Try[Seq[Data]] = mod map (_.simMarkov(0.1).steps.take(100).toSeq)

//   // calculate the mll using a bootstrap filter
//   def mll: Parameters => Try[LogLikelihood] = p => for {
//     mod <- LinearModel(stepBrownian)(p)
//     filter = Filter(mod, ParticleFilter.multinomialResampling)
//     observations <- data
//   } yield filter.llFilter(observations, 0.0)(100)

//   // alright, now we have a function of type A => F[B], what can we do
//   // literally no idea how this can apply to this situation
//   def f: Kleisli[Try, Parameters, LogLikelihood] = Kleisli { mll }
// }
