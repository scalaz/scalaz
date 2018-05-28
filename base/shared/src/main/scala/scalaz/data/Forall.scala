package scalaz
package data

import scala.{ Any, AnyVal }

import scalaz.types.As

trait ForallModule {
  type Forall[F[_]]

  type ∀[F[_]] = Forall[F]

  trait Prototype[F[_]] {
    def apply[A]: F[A]
    final def make: ∀[F] = from(this)
  }

  def specialize[F[_], A](f: ∀[F]): F[A]

  def from[F[_]](p: Prototype[F]): ∀[F]

  def of[F[_]]: MkForall[F]

  def mk[X](implicit u: Unapply[X]): MkForall[u.F] = of[u.F]

  def instantiation[F[_], A]: ∀[F] <~< F[A]

  def vacuous[A]: A <~< ∀[λ[α => A]]

  /** The name monotonicity becomes more apparent if we define a type alias
   * `type <~~<[F[_], G[_]] = ∀[λ[α => F[α] <~< G[α]]]`.
   * Then the signature reads as
   *
   *   `F <~~< G` implies `∀[F] <~< ∀[G]`.
   */
  def monotonicity[F[_], G[_]](ev: ∀[λ[α => F[α] <~< G[α]]]): ∀[F] <~< ∀[G]

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
  }
}

trait ForallSyntax {
  implicit class Ops[F[_]](val a: ∀[F]) {
    def of[A]: F[A]    = Forall.specialize(a)
    def apply[A]: F[A] = of[A]
  }
}

object ForallModule extends ForallSyntax

private[data] object ForallImpl extends ForallModule with ForallSyntax {
  type Forall[F[_]] = F[Any]

  def from[F[_]](p: Prototype[F]): ∀[F] = p[Any]

  def specialize[F[_], A](f: ∀[F]): F[A] = f.asInstanceOf[F[A]]

  def of[F[_]]: MkForall[F] = new MkForallImpl[F]

  def instantiation[F[_], A]: ∀[F] <~< F[A] = As.unsafeForce

  def vacuous[A]: A <~< ∀[λ[α => A]] = As.refl[A]

  // Justification:
  // Having evidence `ev` that `F[α]` is a subtype of `G[α]` for all types `α`,
  // by [[#instantiation]] and transitivity of subtyping ([[Liskov#trans]])
  // we have that `∀[F]` is a subtype of `G[α]` for all types `α`.
  // That means that any term `f: ∀[F]` can be used to create an instance of `∀[G]`,
  // via `of[G].from(f): ∀[G]`, and this is identity on `f`.
  // We have shown that any value `f: ∀[F]` can be used where `∀[G]` is required,
  // which is the very idea of Liskov substitution principle.
  def monotonicity[F[_], G[_]](ev: ∀[λ[α => F[α] <~< G[α]]]): ∀[F] <~< ∀[G] = As.unsafeForce
}

private[data] final class MkForallImpl[F[_]](val dummy: Boolean = false) extends AnyVal with ForallImpl.MkForall[F] {
  type T = Any
  def from(ft: F[T]): ForallImpl.∀[F] = ft
}
