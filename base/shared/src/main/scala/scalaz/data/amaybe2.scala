package scalaz
package data

import scala.{ AnyVal, Nothing }

import scalaz.types.Is

/**
 * Isomorphic to `AMaybe[λ[(α, β) => APair[F[α, ?], G[?, β]]], A, B]`,
 * but avoids allocating an `APair` instance.
 */
sealed abstract class AMaybe2[F[_, _], G[_, _], A, B]

case class AJust2[F[_, _], G[_, _], A, X, B](f: F[A, X], g: G[X, B]) extends AMaybe2[F, G, A, B] {
  type Pivot = X
}

abstract case class AEmpty2[F[_, _], G[_, _], A, B]() extends AMaybe2[F, G, A, B] {
  def subst[H[_]](ha: H[A]): H[B]
  def unsubst[H[_]](hb: H[B]): H[A]
  def leibniz: A === B = subst[A === ?](Is.refl)
}

object AMaybe2 {
  def empty[F[_, _], G[_, _], A](): AMaybe2[F, G, A, A]                            = None.asInstanceOf[AMaybe2[F, G, A, A]]
  def just[F[_, _], G[_, _], A, X, B](f: F[A, X], g: G[X, B]): AMaybe2[F, G, A, B] = AJust2(f, g)

  def apply[F[_, _], G[_, _], A](): AMaybe2[F, G, A, A]                             = empty[F, G, A]
  def apply[F[_, _], G[_, _], A, X, B](f: F[A, X], g: G[X, B]): AMaybe2[F, G, A, B] = just(f, g)
  def of[F[_, _], G[_, _]]: MkAMaybe2[F, G]                                         = new MkAMaybe2[F, G]

  final class MkAMaybe2[F[_, _], G[_, _]](private val dummy: Boolean = false) extends AnyVal {
    def apply[A, X, B](f: F[A, X], g: G[X, B]): AMaybe2[F, G, A, B] = just(f, g)
    def apply[A](): AMaybe2[F, G, A, A]                             = empty[F, G, A]
  }

  private val None = none[Nothing, Nothing, Nothing]
  private def none[F[_, _], G[_, _], A]: AMaybe2[F, G, A, A] = new AEmpty2[F, G, A, A] {
    def subst[H[_]](ha: H[A]): H[A]   = ha
    def unsubst[H[_]](hb: H[A]): H[A] = hb
  }
}
