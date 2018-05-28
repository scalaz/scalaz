package scalaz
package data

import scala.{ Option, Some }

/**
 * Type-aligned pair with upper-bounded type parameter. Isomorphic to
 *
 * {{{
 * (F[A], G[A]) forSome { type A <: U }
 * }}}
 *
 * but more robust with respect to type inference.
 */
sealed abstract class BoundedAPair[U, F[_ <: U], G[_ <: U]] {
  type Pivot <: U
  val _1: F[Pivot]
  val _2: G[Pivot]
}

object BoundedAPair {
  def apply[U, F[_ <: U], G[_ <: U], A <: U](fa: F[A], ga: G[A]): BoundedAPair[U, F, G] =
    new BoundedAPair[U, F, G] { type Pivot = A; val _1 = fa; val _2 = ga }

  def unapply[U, F[_ <: U], G[_ <: U]](p: BoundedAPair[U, F, G]): Option[(F[p.Pivot], G[p.Pivot])] =
    Some((p._1, p._2))

  /** Defer specifying `A`, so that it could possibly be inferred. */
  def of[U, F[_ <: U], G[_ <: U]]: Builder[U, F, G] = new Builder[U, F, G]

  class Builder[U, F[_ <: U], G[_ <: U]] {
    def apply[A <: U](fa: F[A], ga: G[A]): BoundedAPair[U, F, G] =
      BoundedAPair[U, F, G, A](fa, ga)
  }
}
