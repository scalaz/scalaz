package scalaz
package data

import scala.language.implicitConversions

trait Forall2Module {
  type Forall2[F[_, _]]

  type ∀∀[F[_, _]] = Forall2[F]

  trait Prototype[F[_, _]] {
    def apply[A, B]: F[A, B]
  }

  def specialize[F[_, _], A, B](f: ∀∀[F]): F[A, B]

  def from[F[_, _]](p: Prototype[F]): ∀∀[F]

  def of[F[_, _]]: MkForall2[F]

  def mk[X](implicit u: Unapply[X]): MkForall2[u.F] = of[u.F]

  sealed trait MkForall2[F[_, _]] extends Any {
    type T
    type U
    def from(ft: F[T, U]): ∀∀[F]
    def apply(ft: F[T, U]): ∀∀[F] = from(ft)
  }

  trait Unapply[X] {
    type F[_, _]
  }

  object Unapply {
    implicit def unapply[G[_, _]]: Unapply[∀∀[G]] { type F[A, B] = G[A, B] } =
      new Unapply[∀∀[G]] { type F[A, B] = G[A, B] }

    implicit def unapply1[G[_], H[_, _]]: Unapply[∀∀[λ[(α, β) => G[H[α, β]]]]] { type F[A, B] = G[H[A, B]] } =
      new Unapply[∀∀[λ[(α, β) => G[H[α, β]]]]] { type F[A, B] = G[H[A, B]] }

    implicit def unapply2[P[_, _], G[_], H[_]]: Unapply[∀∀[λ[(α, β) => P[G[α], H[β]]]]] { type F[A, B] = P[G[A], H[B]] } =
      new Unapply[∀∀[λ[(α, β) => P[G[α], H[β]]]]] { type F[A, B] = P[G[A], H[B]] }

    implicit def unapply3[P[_, _], G[_, _], H[_, _]]: Unapply[∀∀[λ[(α, β) => P[G[α, β], H[α, β]]]]] { type F[A, B] = P[G[A, B], H[A, B]] } =
      new Unapply[∀∀[λ[(α, β) => P[G[α, β], H[α, β]]]]] { type F[A, B] = P[G[A, B], H[A, B]] }
  }
}

trait Forall2Syntax {
  import Forall2Syntax._

  implicit def toForall2Ops[F[_, _]](a: ∀∀[F]): Ops[F] = new Ops[F](a)
  implicit def toForall2Ops1[F[_], G[_, _]](a: ∀∀[λ[(α, β) => F[G[α, β]]]]): Ops[λ[(α, β) => F[G[α, β]]]] = new Ops[λ[(α, β) => F[G[α, β]]]](a)
  implicit def toForall2Ops2[F[_, _], G[_], H[_]](a: ∀∀[λ[(α, β) => F[G[α], H[β]]]]): Ops[λ[(α, β) => F[G[α], H[β]]]] = new Ops[λ[(α, β) => F[G[α], H[β]]]](a)
  implicit def toForall2Ops3[F[_, _], G[_, _], H[_, _]](a: ∀∀[λ[(α, β) => F[G[α, β], H[α, β]]]]): Ops[λ[(α, β) => F[G[α, β], H[α, β]]]] = new Ops[λ[(α, β) => F[G[α, β], H[α, β]]]](a)
  // add other shapes here as needed
}

object Forall2Syntax {
  final class Ops[F[_, _]](val a: ∀∀[F]) extends AnyVal {
    def of[A, B]: F[A, B] = Forall2.specialize(a)
    def apply[A, B]: F[A, B] = of[A, B]
  }
}

private[data] object Forall2Impl extends Forall2Module with Forall2Syntax {
  type Forall2[F[_, _]] = F[Any, Any]

  def from[F[_, _]](p: Prototype[F]): ∀∀[F] = p[Any, Any]

  def specialize[F[_, _], A, B](f: ∀∀[F]): F[A, B] = f.asInstanceOf[F[A, B]]

  def of[F[_, _]]: MkForall2[F] = new MkForall2Impl[F]
}

private[data] final class MkForall2Impl[F[_, _]](val dummy: Boolean = false) extends AnyVal with Forall2Impl.MkForall2[F] {
  type T = Any
  type U = Any
  def from(ft: F[T, U]): Forall2Impl.∀∀[F] = ft
}

