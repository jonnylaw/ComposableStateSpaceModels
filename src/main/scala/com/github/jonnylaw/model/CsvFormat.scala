package com.github.jonnylaw.model

import breeze.linalg.DenseVector
import cats.Show

// various shows for csv formatting
object CsvFormatShow {
  implicit val stateShow = new Show[State] {
    def show(a: State): String = a match {
      case Branch(l, r) => show(l) + ", " + show(r)
      case Leaf(x)      => x.data.mkString(", ")
      case Empty        => ""
    }
  }

  implicit def dataShow(implicit S: Show[State]) = new Show[Data] {
    def show(a: Data): String = a match {
      case TimedObservation(t, y) => s"$t, ${y.getOrElse("NA")}"
      case ObservationWithState(t, y, e, g, x) => s"$t, ${y.getOrElse("NA")}, $e, $g, ${S.show(x)}"
      case TimestampObservation(time, t, obs) => s"$time, ${obs.getOrElse("NA")}"
    }
  }

  implicit def decompShow = new Show[DecomposedModel] {
    def show(a: DecomposedModel): String = s"${a.time}, ${a.observation}, ${a.eta}, ${a.gamma}, ${a.state.mkString(", ")}"
  }

  implicit def dvShow = new Show[DenseVector[Double]] {
    def show(dv: DenseVector[Double]) = dv.data.mkString(", ")
  }

  implicit def sdeParamShow(implicit S: Show[DenseVector[Double]]) = new Show[SdeParameter] {
    def show(p: SdeParameter): String = p match {
      case GenBrownianParameter(m0, c0, mu, sigma) =>
        s"""${S.show(m0)}, ${S.show(c0)}, ${S.show(mu)}, ${S.show(sigma)}"""
      case BrownianParameter(m0, c0, sigma) =>
        s"""${S.show(m0)}, ${S.show(c0)}, ${S.show(sigma)}"""
      case OuParameter(m, c, a, s, t) => s"${S.show(m)}, ${S.show(c)}, ${S.show(a)}, ${S.show(s)}, ${S.show(t)}"
    }
  }

  implicit def parameterShow(implicit S: Show[SdeParameter]) = new Show[Parameters] {
    def show(p: Parameters): String = p match {
      case Leaf(ParamNode(v, sde)) => v.map(x => s"$x, ").getOrElse("") + S.show(sde)
      case Branch(l, r) => show(l) + ", " + show(r)
      case Empty => ""
    }
  }

  implicit def stateSpaceShow(implicit S: Show[State]) = new Show[StateSpace[State]] {
    def show(a: StateSpace[State]): String = s"${a.time}, ${S.show(a.state)}"
  }

  implicit def itersShow(implicit S: Show[Parameters], T: Show[StateSpace[State]]) = new Show[MetropState[Parameters, State]] {
    def show(a: MetropState[Parameters, State]): String =
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def paramStateShow(implicit S: Show[Parameters]) = new Show[ParamsState[Parameters]] {
    def show(a: ParamsState[Parameters]): String = 
      s"${S.show(a.params)}, ${a.accepted}"
  }

  implicit def filterShow(implicit S: Show[State]) = new Show[PfState[State]] {
    def show(a: PfState[State]): String = a.observation match {
      case Some(y) => s"${a.t}, $y, ${a.particles.map(S.show).mkString(", ")}, ${a.ess}"
      case None => s"${a.t}, NA, ${a.particles.map(S.show).mkString(", ")}, ${a.ess}"
    }
  }

  implicit def credibleIntervalsShow = new Show[CredibleInterval] {
    def show(a: CredibleInterval): String = s"${a.lower}, ${a.upper}"
  }

  implicit def filterOutShow(implicit S: Show[State], C: Show[CredibleInterval]) = new Show[PfOut[State]] {
    def show(a: PfOut[State]): String = a.observation match {
      case Some(x) =>
        s"${a.time}, $x, ${a.eta}, ${C.show(a.etaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
      case None =>
        s"${a.time}, NA, ${a.eta}, ${C.show(a.etaIntervals)}, ${S.show(a.state)}, ${a.stateIntervals.map(C.show).mkString(", ")}"
    }
  }

  implicit def forecastOutShow(implicit S: Show[State]) = new Show[ForecastOut[State]] {
    def show(a: ForecastOut[State]): String = s"${a.t}, ${a.obs}, ${a.obsIntervals.toString}, ${a.eta}, ${a.etaIntervals.toString}, ${S.show(a.state)}, ${a.stateIntervals.mkString(", ")}"
  }
}
