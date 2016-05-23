package model

import model.POMP._

/**
  * Created by jonny on 12/12/2015.
  */
sealed trait State {
  import State._

  def map(f: Vector[Double] => Vector[Double]): State = State.map(this)(f)
  def mapValues(f: Double => Double): State = State.mapValues(this)(f)
  def size: Int = State.size(this)
  def append(a: Vector[Double]): State = State.append(this, a)
  def head: Double = State.head(this)
  def flatten: Vector[Double] = State.flattenState(this)
  def |+|(that: State): State = {
    combineState(this, that)
  }
  override def toString: String = this.flatten.mkString(", ")
}
case class LeafState(x: Vector[Double]) extends State with Serializable {
  def isEmpty: Boolean = this.x.isEmpty
}
case class BranchState(left: State, right: State) extends State with Serializable

object State {
  def combineState(state1: State, state2: State): State = {
    BranchState(state1, state2)
  }


  /** Returns the value of the head of the leftmost state */
  def head(s: State): Double = s match {
    case LeafState(x) => x.head
    case BranchState(ls, _) => head(ls)
  }

  /**
    * Append to the left of a state
    */
  def append(s: State, a: Vector[Double]): State = s match {
    case LeafState(x) => LeafState(a ++ x)
    case BranchState(ls, rs) => BranchState(append(ls, a), rs)
    }

  /**
    * Maps all the values contained inside of all leaf nodes
    * @param s a given tree of states
    * @param f a function from a vector to a vector, usually defined using map, eg. x => x map (_ + 1)
    * @return the state in the same structure only changed by the provided f
    */
  def map(s: State)(f: Vector[Double] => Vector[Double]): State = s match {
    case BranchState(l, r) => BranchState(map(l)(f), map(r)(f))
    case LeafState(x) => LeafState(f(x))
  }

  /**
    * Use map to map the individual values of the state
    */
  def mapValues(s: State)(f: Double => Double): State = s match {
    case BranchState(l, r) => BranchState(map(l)(a => a map f), map(r)(a => a map f))
    case LeafState(x) => LeafState(x map f)
  }

  /**
    * Calculate the mean of a vector of states and return a state in the same structure
    * 
    */
  def stateMean(s: Vector[State]): State = {
    val addedStates = s reduceLeft addStates
    mapValues(addedStates)(a => a/s.size)
    }

  /**
    * Calculate the weighted mean of a list of States
    * @param x a list of States
    * @param w their associated weights
    * @return the weighted mean
    */
  def weightedMean(
    x: Vector[State],
    w: Vector[Double]): State = {
    val normalisedWeights = w map (_ / w.sum)
    val st = x.zip(normalisedWeights) map {
      case (s, weight) =>
        def loop(s: State, w: Double): State =
          s match {
            case LeafState(state) => LeafState(state map (_ * w))
            case BranchState(ls, rs) => BranchState(loop(ls, w), loop(rs, w))
          }
        loop(s, weight)
    }
    st.reduceLeft((a: State, b: State) => addStates(a,b))
  }

  /**
    * Add two states with the same structure, used in weighted mean
    * 
    */
  def addStates(
    s1: State,
    s2: State): State =

    (s1, s2) match {
      case (x: LeafState, y: LeafState) if x.isEmpty => y
      case (x: LeafState, y: LeafState) if y.isEmpty => x
      case (LeafState(x), LeafState(x1)) => LeafState(x.zip(x1) map { case (a,b) => a + b })
      case (BranchState(l, r), BranchState(l1, r1)) => BranchState(addStates(l, l1), addStates(r, r1))

      // These two scenarios should not occur
      case (x: LeafState, BranchState(ls, rs)) => addStates(x, addStates(ls, rs))
      case (BranchState(ls, rs), x: LeafState) => addStates(addStates(ls, rs), x)
    }


  /**
    * Maps the leftmost value of a tree
    * @param s a tree of states
    * @param f a function from a vector to a vector, usually defined using map, eg. x => x map (_ + 1)
    * @return the state with the same structure, with the leftmost value changed by f
    */
  def mapLeft(s: State)(f: Vector[Double] => Vector[Double]): State = s match {
    case BranchState(l: LeafState, r) => BranchState(mapLeft(l)(f), r)
    case BranchState(l, r) => BranchState(mapLeft(l)(f), r)
    case LeafState(x) => LeafState(f(x))
  }

  /**
    * Prints the value on the leftmost leaf
    * @param s a tree of states
    * @return a Single state contained in the left side of the tree
    */
  def getLeft(s: State): State = s match {
    case BranchState(l, r) => getLeft(l)
    case x: LeafState => x
  }

  /**
    * Converts the tree into a list
    * @param s a tree of states
    * @return a list of vectors
    */
  def toList(s: State): List[Vector[Double]] = s match {
    case BranchState(l, r) => toList(l) ::: toList(r)
    case LeafState(x) => List(x)
  }

  /**
    * foldLeft should combine all the elements together into a type A
    * @param s a tree of states
    * @param z a data structure of type a
    * @param f a function combining the type a and the contents of a LeafState
    * @return a type A
    */
  def foldLeft[A](s: State)(z: A)(f: (A, Vector[Double]) => A): A = {
    s match {
      case LeafState(x) => f(z, x)
      case BranchState(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }
  }

  /**
    * Sum the values of the states at the leaves
    * @param s a state
    * @return the values of the states added together
    */
  def sumState(s: State): Double = {
    foldLeft(s)(0.0)((acc, b) => acc + b.sum)
  }

  def observeLeft(s: State)(f: Vector[Double] => Observation): Observation = s match {
    case BranchState(l, r) => observeLeft(l)(f)
    case LeafState(x) => f(x)
  }

  /**
    * Get the node element at position n from the left, indexed from 0
    * @param n the node position from the left
    */
  def getState(s: State, n: Int): LeafState = {
    val l = toList(s)
    LeafState(l(n))
  }

  def flattenState(s: State): Vector[Double] =
    s match {
      case LeafState(x) => x
      case BranchState(ls, rs) => flattenState(ls) ++ flattenState(rs)
    }

  def size(s: State): Int = toList(s).flatten.length
}
