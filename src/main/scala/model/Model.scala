package model

import model.DataTypes._
import java.io.Serializable
import model.POMP._
import model.Model._
import model.StateSpace._
import breeze.stats.distributions.{Rand, Density}
import breeze.linalg.DenseVector
import cats._
import cats.implicits._
import cats.std.all._

trait Model {
  // The observation model
  def observation: Eta => (Rand[Observation] with Density[Observation])
  // the link function
  def link(x: Gamma): Eta = Vector(x)
  // deterministic transformation, such as seasonality
  def f(s: State, t: Time): Gamma
  // initialise the SDE state
  def x0: Rand[State]
  // Step the SDE
  def stepFunction: (State, TimeIncrement) => Rand[State]
  // calculate the likelihood of the observation given the state
  def dataLikelihood: (Eta, Observation) => LogLikelihood
}

object Model {
  def op(mod1: Parameters => Model, mod2: Parameters => Model): Parameters => Model = p => new Model {

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

trait UnparamModel extends (Parameters => Model)

object UnparamModel {
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
