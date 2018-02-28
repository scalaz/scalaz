package scalaz
package data

import scala.annotation.tailrec

/**
 * Non-empty type-aligned sequence represented as a (non-balanced) binary tree,
 * supporting O(1) addition to either end and O(1) concatenation.
 */
sealed abstract class ACatenable1[=>:[_, _], A, B] {
  import ACatenable1._

  def compose[Z](that: ACatenable1[=>:, Z, A]): ACatenable1[=>:, Z, B] =
    Chain(that, this)

  def <<<[Z](that: ACatenable1[=>:, Z, A]): ACatenable1[=>:, Z, B] =
    this compose that

  def >>>[C](that: ACatenable1[=>:, B, C]): ACatenable1[=>:, A, C] =
    that compose this

  def :+[C](f: B =>: C): ACatenable1[=>:, A, C] =
    Chain(this, Lift(f))

  def +:[Z](f: Z =>: A): ACatenable1[=>:, Z, B] =
    Chain(Lift(f), this)

  final def foldLeft[F[_]](fa: F[A])(φ: RightAction[F, =>:]): F[B] = {
    @tailrec def go[X](fx: F[X], tail: ACatenable1[=>:, X, B]): F[B] =
      tail match {
        case Chain(Chain(f, g), h) => go(fx, f >>> (g >>> h))
        case Chain(Lift(f), g)     => go(φ.apply(fx, f), g)
        case Lift(f)               => φ.apply(fx, f)
      }

    go(fa, this)
  }

  def foldRight[F[_]](fb: F[B])(φ: LeftAction[F, =>:]): F[A] = {
    @tailrec def go[X](init: ACatenable1[=>:, A, X], fx: F[X]): F[A] =
      init match {
        case Chain(f, Chain(g, h)) => go((f >>> g) >>> h, fx)
        case Chain(f, Lift(g))     => go(f, φ.apply(g, fx))
        case Lift(f)               => φ.apply(f, fx)
      }

    go(this, fb)
  }

  /**
   * Compose the leafs in a balanced binary fashion.
   */
  @tailrec
  final def fold(implicit ev: Compose[=>:]): A =>: B = this match {
    case Chain(Chain(f, g), h) => (f >>> (g >>> h)).fold
    case Chain(Lift(f), g) =>
      g.foldLeft[PostComposeBalancer[=>:, A, ?]](PostComposeBalancer(f))(PostComposeBalancer.rightAction).result
    case Lift(f) => f
  }
}

object ACatenable1 {
  private[ACatenable1] final case class Lift[=>:[_, _], A, B](f: A =>: B) extends ACatenable1[=>:, A, B]
  private[ACatenable1] final case class Chain[=>:[_, _], A, B, C](f: ACatenable1[=>:, A, B], g: ACatenable1[=>:, B, C])
      extends ACatenable1[=>:, A, C]

  def lift[F[_, _], A, B](f: F[A, B]): ACatenable1[F, A, B] =
    Lift(f)
}
