package model

import model.POMP._
import model.DataTypes._
import breeze.stats.distributions.{Rand, Gaussian}
import breeze.numerics.{exp, sqrt}

object StateSpace {
    /**
    * Steps all the states using the identity
    * @param p a Parameter
    * @return a function from state, dt => State
    */
  def stepNull(p: Parameters): (State, TimeIncrement) => Rand[State] = {
    (s, dt) => new Rand[State] { def draw = State.map(s)(x => x) }
  }

  /**
    * A step function for generalised brownian motion, dx_t = mu dt + sigma dW_t
    * @param p a set of parameters, we are expecting a single Parameter
    * @return A function from state, time increment to state
    */
  def stepBrownian(p: SdeParameter): (State, TimeIncrement) => Rand[State] = {
    (s, dt) => (p, s) match {
      case (BrownianParameter(mu, sigma), LeafState(x)) => 
        new Rand[State] {
          def draw = {
            LeafState(
              (x, mu, sigma).zipped map { case (a, m, sd) =>
                Gaussian(a + m * dt, Math.sqrt(sd * sd * dt)).draw
              })
          }
        }
    }
  }
    /**
    * Steps the leftmost state by the value of the parameter "a" 
    * multiplied by the time increment "dt"
    * @param p a parameter Map
    * @return a function from (State, dt) => State, with the
    * states being the same structure before and after
    */
  def stepConstant(p: SdeParameter): (State, TimeIncrement) => Rand[State] = {
    (s, dt) => (s, p) match {
      case (LeafState(state), StepConstantParameter(a)) => 
          new Rand[State] {
            def draw = {
              LeafState((state, a).zipped map { case (x, a) => x + a*dt })
            }
          }
    }
  }

  /**
    * A step function for the Ornstein Uhlenbeck process dx_t = - alpha x_t dt + sigma dW_t
    * @param p the parameters of the ornstein uhlenbeck process, theta, alpha and sigma
    * @return
    */
  def stepOrnstein(p: SdeParameter): (State, TimeIncrement) => Rand[State] = {
    (s, dt) => (p, s) match {
      case (OrnsteinParameter(theta, alpha, sigma), LeafState(x)) => // determine we have received the correct parameters and state
            new Rand[State] {
              def draw = {
                // calculate the mean of the solution
                val mean = x.zip(alpha) map { case (a, b) => a * exp(- b * dt) }
                // calculate the variance of the solution
                val variance = sigma.zip(alpha) map { case (s, a) => (s*s/2*a)*(1-exp(-2*a*dt)) }
                LeafState(mean.zip(variance) map { case (a, v) => Gaussian(a, sqrt(v)).draw() })
              }
            }
      }
    }

  def stepCIR(p: SdeParameter): (State, TimeIncrement) => Rand[State] = ???

}
