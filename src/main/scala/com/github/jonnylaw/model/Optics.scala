// package com.github.jonnylaw.model

// import monocle.{Prism, Lens, Traversal}
// import monocle.macros.GenLens
// import breeze.linalg.DenseVector
// import scalaz.Traverse

// object Optics {
//   def sigmaLens = GenLens[BrownianParameter](_.sigma)
//   def brownianPrism = Prism.partial[SdeParameter, BrownianParameter]{ case p: BrownianParameter => p }(identity)
//   def sdeLens = GenLens[LeafParameter](_.sdeParam)
//   def leafPrism = Prism.partial[Parameters, LeafParameter]{ case p: LeafParameter => p }(identity)
//   def sdePrism = leafPrism composeLens sdeLens
//   def sigmaPrism = brownianPrism composeLens sigmaLens

//   /**
//     * Extract a value from a leaf node
//     */
//   def leafValue[A] = Prism.partial[Tree[A], A]{ case Leaf(value) => value }(Tree.leaf)

//   /**
//     * Function to update sigma from a lens
//     * @param newValue the new value of sigma to replace in a Leaf Parameter
//     */
//   def updateSigma(newValue: DenseVector[Double])(p: LeafParameter): Option[Parameters] = for {
//     bp <- (sdeLens composePrism brownianPrism).getOption(p)
//     sigma = sigmaLens.set(newValue)(bp)
//   } yield sdePrism.set(brownianPrism.set(sigma)(bp))(p)


//   implicit val paramTraverse = new Traverse[Parameters] {
//     def foldRight[A,B](fa: Tree[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = ???

//     def foldLeft[A,B](fa: Tree[A], z: B)(f: (B, A) => B): B = ???

//     def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] = fa match {
//       case Leaf(value) => G.map(f(value))(Tree.leaf(_))
//       case Branch(l, r) => G.map2(traverse(l)(f), traverse(r)(f))(_ +++ _)
//       case Empty => G.pure(Tree.empty)
//     }
//   }
// }
