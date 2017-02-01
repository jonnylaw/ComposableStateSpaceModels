// package com.github.jonnylaw.model

// import ParticleFilter._
// import DataTypes._
// import breeze.linalg.DenseVector
// import cats._
// import breeze.stats.distributions.Rand

// sealed trait State {
//   import State._

//   def map(f: DenseVector[Double] => DenseVector[Double]): State = State.map(this)(f)
//   def flatten: Seq[Double] = State.flatten(this)
//   def head: Double = State.head(this)
//   def isEmpty: Boolean = State.isEmpty(this)
//   override def toString: String = this.flatten.mkString(", ")
// }

// case class LeafState(data: DenseVector[Double]) extends State
// case class BranchState(left: State, right: State) extends State

// object LeafState {
//   def apply(a: Double): LeafState = {
//     new LeafState(DenseVector(a))
//   }
// }

// object State {
//   implicit def stateMonoid = new Monoid[State] {
//     override def combine(s1: State, s2: State): State = State.combine(s1, s2)
//     override def empty: State = State.zero
//   }

//   def combine(state1: State, state2: State): State = {
//     if (state1.isEmpty) {
//       state2
//     } else if (state2.isEmpty) {
//       state1
//     } else {
//       BranchState(state1, state2)
//     }
//   }

//   val zero: State = {
//     LeafState(DenseVector[Double]())
//   }

//   def head(s: State): Double = s match {
//     case LeafState(x) => x(0)
//     case BranchState(l, _) => head(l)
//   }

//   /**
//     * Determines if a state contains 
//     */
//   def isEmpty(state: State): Boolean = state match {
//     case LeafState(x) => x.length == 0
//     case BranchState(lp, rp) => isEmpty(lp) && isEmpty(rp)
//   }

//   def toList(s: State): List[DenseVector[Double]] = s match {
//     case BranchState(l, r) => toList(l) ::: toList(r)
//     case LeafState(x) => List(x)
//   }

//   /**
//     * Get the node element at position n from the left, indexed from 0
//     * @param n the node position from the left
//     */
//   def getState(s: State, n: Int): LeafState = {
//     val l = toList(s)
//     LeafState(l(n))
//   }

//   /**
//     * Maps all the values contained inside of all leaf nodes
//     * @param s a given tree of states
//     * @param f a function from a vector to a vector, usually defined using map, eg. x => x map (_ + 1)
//     * @return the state in the same structure only changed by the provided f
//     */
//   def map(s: State)(f: DenseVector[Double] => DenseVector[Double]): State = s match {
//     case LeafState(x) => LeafState(f(x))
//     case BranchState(l, r) => BranchState(map(l)(f), map(r)(f))
//   }

//   /**
//     * Calculate the weighted mean of a list of States
//     * @param x a list of States
//     * @param w their associated weights
//     * @return the weighted mean
//     */
//   def weightedMean(x: Seq[State], w: Seq[Double]): State = {

//     val normalisedWeights = w map (_ / w.sum)
//     val st = x.zip(normalisedWeights) map {
//       case (s, weight) =>
//         def loop(s: State, w: Double): State =
//           s match {
//             case LeafState(state) => LeafState(state map (_ * w))
//             case BranchState(ls, rs) => BranchState(loop(ls, w), loop(rs, w))
//           }
//         loop(s, weight)
//     }
//     st.reduceLeft((a: State, b: State) => addStates(a,b))
//   }

//   def weightedMean2(x: Seq[State], w: Seq[Double]): State = {
//     val normaliseWeights = w map (_ / w.sum)

//     x.zip(normaliseWeights).
//       map { case (state, weight) => state map (_ * weight) }.
//       reduce(addStates)
//   }

//   /**
//     *  Calculate the mean of a state
//     */
//   def meanState(x: Seq[State]): State = {
//     weightedMean(x, Seq.fill(x.length)(1))
//   }

//   /**
//     * Get the credible intervals of the nth state vector
//     * @param s a State
//     * @param n a reference to a node of state tree, counting from 0 on the left
//     * @param interval the probability interval size
//     * @return a tuple of doubles, (lower, upper)
//     */
//   def getCredibleInterval(s: Seq[State], n: Int, interval: Double): IndexedSeq[CredibleInterval] = {
//     val state = s map (State.getState(_, n)) // Gets the nth state vector
//     val stateVec = state.head.data.data.toVector.indices map (i => state.map(a => a.data(i)))
//     stateVec map (a => {
//       val index = Math.floor(interval * a.length).toInt
//       val stateSorted = a.sorted
//       CredibleInterval(stateSorted(a.length - index - 1), stateSorted(index - 1))
//     })
//   }

//   /**
//     * Use getCredibleInterval to get all credible intervals of a state
//     * @param s a vector of states
//     * @param interval the interval for the probability interval between [0,1]
//     * @return a sequence of tuples, (lower, upper) corresponding to each state reading
//     */
//   def getAllCredibleIntervals(s: Seq[State], interval: Double): IndexedSeq[CredibleInterval] = {
//     State.toList(s.head).indices.flatMap(i => getCredibleInterval(s, i, interval))
//   }


//   /**
//     * Given a distribution over State, calculate credible intervals by 
//     * repeatedly drawing from the distribution and ordering the samples
//     */
//   def getCredibleIntervals(x0: Rand[State], interval: Double): IndexedSeq[CredibleInterval] = {
//     getAllCredibleIntervals(x0.sample(1000), interval)
//   }

//   /**
//     * Add two states with the same structure, used in weighted mean
//     */
//   def addStates(
//     s1: State,
//     s2: State): State =

//     (s1, s2) match {
//       case (x: LeafState, y: LeafState) if x.isEmpty => y
//       case (x: LeafState, y: LeafState) if y.isEmpty => x
//       case (LeafState(x), LeafState(x1)) => LeafState(x + x1)
//       case (BranchState(l, r), BranchState(l1, r1)) => BranchState(addStates(l, l1), addStates(r, r1))
//     }

//   def flatten(s: State): Seq[Double] =
//     s match {
//       case LeafState(x) => x.data
//       case BranchState(ls, rs) => flatten(ls) ++ flatten(rs)
//     }
// }
