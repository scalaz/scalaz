package scalaz
package data

import scala.{ Any, AnyVal }

import Predef._

trait Forall3Module {
  type Forall3[F[_, _, _]]

  type ∀∀∀[F[_, _, _]] = Forall3[F]

  trait Prototype[F[_, _, _]] {
    def apply[A, B, C]: F[A, B, C]
    final def make: ∀∀∀[F] = from(this)
  }

  def specialize[F[_, _, _], A, B, C](f: ∀∀∀[F]): F[A, B, C]

  def from[F[_, _, _]](p: Prototype[F]): ∀∀∀[F]

  def of[F[_, _, _]]: MkForall3[F]

  def mk[X](implicit u: Unapply[X]): MkForall3[u.F] = of[u.F]

  sealed trait MkForall3[F[_, _, _]] extends Any {
    type T
    type U
    type V
    def from(ft: F[T, U, V]): ∀∀∀[F]
    def apply(ft: F[T, U, V]): ∀∀∀[F] = from(ft)
  }

  trait Unapply[X] {
    type F[_, _, _]
  }

  object Unapply {
    implicit def unapply[G[_, _, _]]: Unapply[∀∀∀[G]] { type F[A, B, C] = G[A, B, C] } =
      new Unapply[∀∀∀[G]] { type F[A, B, C] = G[A, B, C] }
  }
}

trait Forall3Syntax {
  implicit final class Ops[F[_, _, _]](val a: ∀∀∀[F]) {
    def of[A, B, C]: F[A, B, C]    = Forall3.specialize(a)
    def apply[A, B, C]: F[A, B, C] = of[A, B, C]
  }
}

object Forall3Module extends Forall3Syntax

private[data] object Forall3Impl extends Forall3Module with Forall3Syntax {
  type Forall3[F[_, _, _]] = F[Any, Any, Any]

  def from[F[_, _, _]](p: Prototype[F]): ∀∀∀[F] = p[Any, Any, Any]

  def specialize[F[_, _, _], A, B, C](f: ∀∀∀[F]): F[A, B, C] = f.asInstanceOf[F[A, B, C]]

  def of[F[_, _, _]]: MkForall3[F] = new MkForall3Impl[F]
}

private[data] final class MkForall3Impl[F[_, _, _]](val dummy: Boolean = false)
    extends AnyVal
    with Forall3Impl.MkForall3[F] {
  type T = Any
  type U = Any
  type V = Any
  def from(ft: F[T, U, V]): Forall3Impl.∀∀∀[F] = ft
}
