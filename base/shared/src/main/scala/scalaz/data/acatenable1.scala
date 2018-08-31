package scalaz
package data

import scala.annotation.tailrec

import tc.{ instanceOf, Semicategory, SemicategoryClass }

/**
 * Non-empty type-aligned sequence represented as a (non-balanced) binary tree,
 * supporting O(1) addition to either end and O(1) concatenation.
 */
sealed abstract class ACatenable1[=>:[_, _], A, B] {
  import ACatenable1._

  def :::[Z](that: ACatenable1[=>:, Z, A]): ACatenable1[=>:, Z, B] =
    Chain(that, this)

  def :++[Z](that: ACatenable1[=>:, B, Z]): ACatenable1[=>:, A, Z] =
    Chain(this, that)

  def +:[Z](f: Z =>: A): ACatenable1[=>:, Z, B] =
    :::(Lift(f))

  def :+[Z](f: B =>: Z): ACatenable1[=>:, A, Z] =
    :++(Lift(f))

  final def foldLeft[F[_]](fa: F[A])(φ: RightAction[F, =>:]): F[B] = {
    @tailrec def go[X](fx: F[X], tail: ACatenable1[=>:, X, B]): F[B] =
      tail match {
        case Chain(Chain(f, g), h) => go(fx, f :++ (g :++ h))
        case Chain(Lift(f), g)     => go(φ.apply(fx, f), g)
        case Lift(f)               => φ.apply(fx, f)
      }

    go(fa, this)
  }

  def foldRight[F[_]](fb: F[B])(φ: LeftAction[F, =>:]): F[A] = {
    @tailrec def go[X](init: ACatenable1[=>:, A, X], fx: F[X]): F[A] =
      init match {
        case Chain(f, Chain(g, h)) => go((f :++ g) :++ h, fx)
        case Chain(f, Lift(g))     => go(f, φ.apply(g, fx))
        case Lift(f)               => φ.apply(f, fx)
      }

    go(this, fb)
  }

  /**
   * Compose the leafs in a balanced binary fashion.
   *
   * Implementation is easiest to understand in terms of `AList1.foldBalanced`;
   * first, we reassociate all left-nested trees to be right-nested. Second,
   * we do exactly what `AList1.foldBalanced` does by folding from the left
   * with `PostComposeBalancer`, because a right-nested `ACatenable1` *is*
   * an `AList1`.
   */
  @tailrec
  final def foldBalanced(implicit ev: Semicategory[=>:]): A =>: B = this match {
    case Chain(Chain(f, g), h) => (f :++ (g :++ h)).foldBalanced
    case Chain(Lift(f), g) =>
      g.foldLeft[PostComposeBalancer[=>:, A, ?]](PostComposeBalancer(f))(PostComposeBalancer.rightAction).result
    case Lift(f) => f
  }

  /**
   * Allows you to view the internal structure of an ACatenable1.
   * Not stack-safe.
   */
  final def fold[G[_, _]](f: Forall3.Prototype[λ[(X, Y, Z) => (G[X, Y], G[Y, Z]) => G[X, Z]]])(z: =>: ~~> G): G[A, B] =
    this match {
      case Chain(a, b) => f.apply(a.fold[G](f)(z), b.fold[G](f)(z))
      case Lift(a)     => z.apply(a)
    }

}

object ACatenable1 {
  private[ACatenable1] final case class Lift[=>:[_, _], A, B](f: A =>: B) extends ACatenable1[=>:, A, B]
  private[ACatenable1] final case class Chain[=>:[_, _], A, B, C](f: ACatenable1[=>:, A, B], g: ACatenable1[=>:, B, C])
      extends ACatenable1[=>:, A, C]

  def lift[F[_, _], A, B](f: F[A, B]): ACatenable1[F, A, B] =
    Lift(f)

  // ACatenable1 is *the* free semicategory in lazy languages like Haskell.
  // AList1 is fine for us, but ACatenable1 is probably faster asymptotically
  // (depending on user code) and never asymptotically slower.
  implicit def acatenable1FreeSemicategory[=>:[_, _]]: Semicategory[ACatenable1[=>:, ?, ?]] =
    instanceOf(new SemicategoryClass[ACatenable1[=>:, ?, ?]] {
      def compose[A, B, C](f: ACatenable1[=>:, B, C], g: ACatenable1[=>:, A, B]): ACatenable1[=>:, A, C] =
        g ::: f
    })
}
