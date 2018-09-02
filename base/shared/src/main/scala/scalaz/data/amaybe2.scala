package scalaz
package data

import scala.{ AnyVal, Nothing }

import Predef._
import prop._
import tc.{ instanceOf, Debug, DebugClass, Eq, EqClass }

/**
 * Isomorphic to `AMaybe[λ[(α, β) => APair[F[α, ?], G[?, β]]], A, B]`,
 * but avoids allocating an `APair` instance.
 */
sealed abstract class AMaybe2[F[_, _], G[_, _], A, B] {
  def fold[Z](empty: (A === B) => Z)(just: Forall[λ[γ => (F[A, γ], G[γ, B]) => Z]]): Z
}

final case class AJust2[F[_, _], G[_, _], A, X, B](f: F[A, X], g: G[X, B]) extends AMaybe2[F, G, A, B] {
  override def fold[Z](empty: (A === B) => Z)(just: Forall[λ[γ => (F[A, γ], G[γ, B]) => Z]]): Z =
    just.apply(f, g)
  type Pivot = X
}

abstract case class AEmpty2[F[_, _], G[_, _], A, B]() extends AMaybe2[F, G, A, B] {
  override final def fold[Z](empty: (A === B) => Z)(just: Forall[λ[γ => (F[A, γ], G[γ, B]) => Z]]): Z =
    empty(leibniz)
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

  implicit def amaybe2Debug[F[_, _], G[_, _], A, B](implicit FAB: Debug[F[A, B]],
                                                    GAB: Debug[G[A, B]]): Debug[AMaybe2[F, G, A, B]] = {
    import Scalaz.debugInterpolator
    import scala.unchecked
    type FAB = F[A, B]
    type GAB = G[A, B]
    DebugClass.instance[AMaybe2[F, G, A, B]] {
      case AJust2(fab: FAB @unchecked, gab: GAB @unchecked) => z"AJust2($fab, $gab)"
      case AEmpty2()                                        => Cord("AEmpty2")
    }
  }

  implicit def amaybe2Eq[F[_, _], G[_, _], A, B](implicit FAB: Eq[F[A, B]],
                                                 GAB: Eq[G[A, B]]): Eq[AMaybe2[F, G, A, B]] = {
    import scala.unchecked
    type FAB = F[A, B]
    type GAB = G[A, B]
    instanceOf[EqClass[AMaybe2[F, G, A, B]]] {
      case (
          AJust2(fab1: FAB @unchecked, gab1: GAB @unchecked),
          AJust2(fab2: FAB @unchecked, gab2: GAB @unchecked)
          ) =>
        FAB.equal(fab1, fab2) && GAB.equal(gab1, gab2)
      case (AEmpty2(), AEmpty2()) =>
        true
      case _ => false
    }
  }
}
