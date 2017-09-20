package scalaz
package data

import scala.annotation.tailrec
import scalaz.typeclass.Compose

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
    @inline def pair[X](fx: F[X], f: ACatenable1[=>:, X, B]) = APair[F, ACatenable1[=>:, ?, B], X](fx, f)
    @tailrec def go(step: APair[F, ACatenable1[=>:, ?, B]]): F[B] =
      step._2 match {
        case Chain(Chain(f, g), h) => go(pair(step._1, f >>> (g >>> h)))
        case Chain(Lift(f), g) => go(pair(φ.apply(step._1, f), g))
        case Lift(f) => φ.apply(step._1, f)
      }
    go(pair(fa, this))
  }

  def foldRight[F[_]](fb: F[B])(φ: LeftAction[F, =>:]): F[A] = {
    @inline def pair[X](f: ACatenable1[=>:, A, X], fx: F[X]) = APair[ACatenable1[=>:, A, ?], F, X](f, fx)
    @tailrec def go(step: APair[ACatenable1[=>:, A, ?], F]): F[A] =
      step._1 match {
        case Chain(f, Chain(g, h)) => go(pair((f >>> g) >>> h, step._2))
        case Chain(f, Lift(g)) => go(pair(f, φ.apply(g, step._2)))
        case Lift(f) => φ.apply(f, step._2)
      }
    go(pair(this, fb))
  }

  /**
   * Compose the leafs in a balanced binary fashion.
   */
  @tailrec
  final def fold(implicit ev: Compose[=>:]): A =>: B = this match {
    case Chain(Chain(f, g), h) => (f >>> (g >>> h)).fold
    case Chain(Lift(f), g) => g.foldLeft[PostComposeBalancer[=>:, A, ?]](PostComposeBalancer(f))(PostComposeBalancer.rightAction).result
    case Lift(f) => f
  }
}

object ACatenable1 {
  private[ACatenable1] final case class Lift[=>:[_, _], A, B](f: A =>: B) extends ACatenable1[=>:, A, B]
  private[ACatenable1] final case class Chain[=>:[_, _], A, B, C](f: ACatenable1[=>:, A, B], g: ACatenable1[=>:, B, C]) extends ACatenable1[=>:, A, C]

  def lift[F[_, _], A, B](f: F[A, B]): ACatenable1[F, A, B] =
    Lift(f)
}
