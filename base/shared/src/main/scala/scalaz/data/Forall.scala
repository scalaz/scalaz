package scalaz
package data

import scala.language.implicitConversions

trait ForallModule {
  type Forall[F[_]]

  type ∀[F[_]] = Forall[F]

  trait Prototype[F[_]] {
    def apply[A]: F[A]
  }

  def specialize[F[_], A](f: ∀[F]): F[A]

  def from[F[_]](p: Prototype[F]): ∀[F]

  def of[F[_]]: MkForall[F]

  def mk[X](implicit u: Unapply[X]): MkForall[u.F] = of[u.F]

  sealed trait MkForall[F[_]] extends Any {
    type T
    def from(ft: F[T]): ∀[F]
    def apply(ft: F[T]): ∀[F] = from(ft)
  }

  trait Unapply[X] {
    type F[_]
  }

  object Unapply {
    implicit def unapply[G[_]]: Unapply[∀[G]] { type F[A] = G[A] } =
      new Unapply[∀[G]] { type F[A] = G[A] }

    implicit def unapply1[G[_], H[_]]: Unapply[∀[λ[α => G[H[α]]]]] { type F[A] = G[H[A]] } =
      new Unapply[∀[λ[α => G[H[α]]]]] { type F[A] = G[H[A]] }

    implicit def unapply2[P[_, _], G[_], H[_]]: Unapply[∀[λ[α => P[G[α], H[α]]]]] { type F[A] = P[G[A], H[A]] } =
      new Unapply[∀[λ[α => P[G[α], H[α]]]]] { type F[A] = P[G[A], H[A]] }
  }
}

trait ForallSyntax {
  import ForallSyntax._

  implicit def toForallOps[F[_]](a: ∀[F]): Ops[F] = new Ops[F](a)
  implicit def toForallOps1[F[_], G[_]](a: ∀[λ[α => F[G[α]]]]): Ops[λ[α => F[G[α]]]] = new Ops[λ[α => F[G[α]]]](a)
  implicit def toForallOps2[F[_, _], G[_], H[_]](a: ∀[λ[α => F[G[α], H[α]]]]): Ops[λ[α => F[G[α], H[α]]]] = new Ops[λ[α => F[G[α], H[α]]]](a)
  // add other shapes here as needed
}

object ForallSyntax {
  final class Ops[F[_]](val a: ∀[F]) extends AnyVal {
    def of[A]: F[A] = Forall.specialize(a)
    def apply[A]: F[A] = of[A]
  }
}

private[data] object ForallImpl extends ForallModule with ForallSyntax {
  type Forall[F[_]] = F[Any]

  def from[F[_]](p: Prototype[F]): ∀[F] = p[Any]

  def specialize[F[_], A](f: ∀[F]): F[A] = f.asInstanceOf[F[A]]

  def of[F[_]]: MkForall[F] = new MkForallImpl[F]
}

private[data] final class MkForallImpl[F[_]](val dummy: Boolean = false) extends AnyVal with ForallImpl.MkForall[F] {
  type T = Any
  def from(ft: F[T]): ForallImpl.∀[F] = ft
}
