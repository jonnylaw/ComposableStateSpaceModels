package model

import model.POMP._
import model.DataTypes._
import breeze.linalg.DenseVector
import breeze.numerics.{exp, log}
import breeze.stats.distributions.Multinomial
import model.Parameters._
import Stream._

object Utilities {
  /**
    * The rectangle method for approximaxing integrals in an interval using rectangles
    */
  def rectangleMethod(start: Double, end: Double, f: Double => Double, precision: Int): Double = {
    val delta = math.pow(10, -precision)
    val x = (start to end by delta)

    delta * (x map f).sum
  }

  /**
    * 
    */
  def rectangleMethodStream(start: Double, end: Double, f: Double => Double, precision: Int): Double = {
    val delta = math.pow(10, -precision)
    val s = iterate((start, f(start))){ case (x, fx) => (x + delta, f(x + delta)) }

    delta * s.takeWhile{ case (s, _) => s < end }.map{ case (_,x) => x }.sum
  }


  /**
   * Rounds a value to a given number of decimal places
   * @param x a Double value of arbitrary precision
   * @param p the number of decimal places to round x
   * @return x rounded to p decimal places
   */
  def precisionRound(x: Double, p: Int): Double = {
    val s = math pow (10, p); (math round x * s) / s
  }

  /**
    * Creates a vector of times between t0 and t with step dt = 10e-precision
    */
  def createTimes(t0: Time, t: Time, precision: Int): Vector[Time] = {
    val dt = math pow (10, -precision)
    val start = precisionRound(t0, precision)
    val end = precisionRound(t, precision)
    def loop(lastt: Time, acc: Vector[Time]): Vector[Time] = {
      if (lastt + dt > end)
        acc.reverse
      else {
        val t1 = precisionRound(lastt + dt, precision)
        loop(t1, t1 +: acc)
      }
    }
    loop(t0, Vector())
  }

  /**
    * Method for adding two vectors together elementwise
    * @param a a vector
    * @param b another vector
    * @return the elementwise sum of the two vectors
    */
  def addVectors(
    a: Vector[Double],
    b: Vector[Double]): Vector[Double] = {
    def loop(a: Vector[Double], b: Vector[Double], sum: Vector[Double]): Vector[Double] = a match {
      case IndexedSeq() => sum
      case x +: IndexedSeq() => sum :+ (b.head + x)
      case x +: xs => loop(xs, b.tail, sum :+ (x + b.head))
    }
    loop(a, b, Vector())
  }
}
