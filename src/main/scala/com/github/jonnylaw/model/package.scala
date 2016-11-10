package com.github.jonnylaw

import breeze.stats.distributions.Rand

package object model {
  type Eta = Vector[Double]
  type Gamma = Double
  type Observation = Double
  type Time = Double
  type TimeIncrement = Double
  type LogLikelihood = Double
  type StepFunction = (SdeParameter) => (State, TimeIncrement) => Rand[State]

  implicit def bool2obs(b: Boolean): Observation = if (b) 1.0 else 0.0
  implicit def obs2bool(o: Observation): Boolean = if (o == 0.0) false else true
}
