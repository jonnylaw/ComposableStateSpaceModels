package com.github.jonnylaw.model

import com.github.jonnylaw.model.DataTypes._
import java.io.Serializable
import com.github.jonnylaw.model.POMP._
import com.github.jonnylaw.model.StateSpace._
import breeze.stats.distributions.{Rand, Density}
import breeze.linalg.DenseVector
import cats._
import cats.implicits._
import cats.std.all._

trait Model {
  /**
    * The observation model, a function from eta to a distribution over the observations
    * realisations can be produced from the observation model by calling draw
    */
  def observation: Eta => Rand[Observation]
  /**
    * The linking-function, transforms the state space into the parameter space of the 
    * observation distribution using a possibly non-linear transformation
    */
  def link(x: Gamma): Eta = Vector(x)
  /**
    * The Linear, deterministic transformation function. f is used to add seasonal factors or
    * other time depending linear transformations
    */ 
  def f(s: State, t: Time): Gamma
  /**
    * Distribution over the initial state of the hidden state which realisations 
    * can be simulated from
    */
  def x0: Rand[State]
  /**
    * An exact or approximate solution to a diffusion process, used to advance the latent state.
    * This function returns a distribution over the next state and can be simulated from
    */
  def stepFunction: (State, TimeIncrement) => Rand[State]
  /**
    * The data likelihood, given a fully transformed latent state, eta, and an observation
    * the log-likelihood can be calculated for use in inference algorithms
    */
  def dataLikelihood: (Eta, Observation) => LogLikelihood
}

trait UnparamModel extends (Parameters => Model)

object UnparamModel {
  /**
    * Models form a monoid, they can be combined to form a composed model
    */
  implicit def modelMonoid = new Monoid[UnparamModel] {
    override def combine(m1: UnparamModel, m2: UnparamModel): UnparamModel =
      UnparamModel.op(m1, m2)

    override def empty: UnparamModel = new UnparamModel{
      def apply(p: Parameters) = {
        new Model {
          def observation = x => new Rand[Observation] with Density[Observation] {
            def draw = x.head
            def apply(x: Observation) = 0.0
          }
          def f(s: State, t: Time) = s.head
          def x0 = new Rand[State] { def draw = LeafState(DenseVector[Double]()) }
          def stepFunction = stepNull(p)
          def dataLikelihood = (s, y) => 0.0
        }
      }
    }
  }

  /**
    * Combine two unparameterised models, usually called with infix notation |+|
    * by importing cats.implicits._, this is not commutative, the observation distribution must 
    * be on the left-hand side of the composition
    * @param mod1 the left-hand model in the composition, if this is a composition of two
    * then the model with the desired observation distribution must be mod1
    * @param mod2 the right-hand model in the composition
    * @return a composed model of mod1 and mod2, which can be composed again
    */
  def op(mod1: UnparamModel, mod2: UnparamModel): UnparamModel = new UnparamModel {
    def apply(p: Parameters) =
      new Model {

        def observation = x => p match {
          case BranchParameter(lp,_) => mod1(lp).observation(x)
          case param: LeafParameter => mod1(param).observation(x)
        }

        override def link(x: Double) = mod1(p).link(x)

        def f(s: State, t: Time) = s match {
          case BranchState(ls, rs) =>
            mod1(p).f(ls, t) + mod2(p).f(rs, t)
          case x: LeafState =>
            mod1(p).f(x, t)
        }

        def x0 = p match {
          case BranchParameter(lp, rp) =>
            for {
              l <- mod1(lp).x0
              r <- mod2(rp).x0
            } yield l |+| r
          case param: LeafParameter =>
            for {
              l <- mod1(param).x0
              r <- mod2(param).x0
            } yield l |+| r
        }

        def stepFunction = (s, dt) => (s, p) match {
          case (BranchState(ls, rs), BranchParameter(lp, rp)) =>
            for {
              l <- mod1(lp).stepFunction(ls, dt)
              r <- mod2(rp).stepFunction(rs, dt)
            } yield BranchState(l, r)
          case (x: LeafState, param: LeafParameter) => // Null model case, non-null must be on left
            mod1(param).stepFunction(x, dt)
          case _ => throw new Exception("Incorrect Parameters or state supplied to composed model stepFunction")
        }

        def dataLikelihood = (s, y) => p match {
          case param: LeafParameter => mod1(param).dataLikelihood(s, y)
          case BranchParameter(lp, _) => mod1(lp).dataLikelihood(s, y)
        }
      }
  }
}
