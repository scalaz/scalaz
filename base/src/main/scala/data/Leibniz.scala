package scalaz
package data

import Prelude.===
import Leibniz.refl

/**
  * A value of `Leibniz[A, B]` is proof that the types `A` and `B` are the same.
  * More powerfully, it asserts that they have the same meaning in all type
  * contexts. This can be a more powerful assertion than `A =:= B` and is more
  * easily used in manipulation of types while avoiding (potentially
  * erroneous) coercions.
  *
  * @see [[===]] `A === B` is a type synonym to `Leibniz[A, B]`
  */
sealed abstract class Leibniz[A, B] private[Leibniz] () { ab =>
  /**
    * To create an instance of `Leibniz[A, B]` you must show that for every
    * choice of `F[_]` you can convert `F[A]` to `F[B]`. Loosely, this reads
    * as saying that `B` must have the same effect as `A` in all contexts
    * therefore allowing type substitution.
    */
  def subst[F[_]](fa: F[A]): F[B]

  /**
    * Substitution on identity brings about a direct coercion function of the
    * same form that `=:=` provides.
    *
    * @see [[coerce]]
    */
  def apply(a: A): B =
    coerce(a)

  /**
    * Equality is transitive and its witnesses can be composed in a
    * chain much like functions.
    */
  def andThen[C](bc: B === C): A === C =
    bc.subst[A === ?](ab)

  /**
    * Equality is transitive and its witnesses can be composed in a
    * chain much like functions.
    *
    * @see [[andThen]]
    */
  def compose[Z](za: Z === A): Z === B =
    za.andThen(ab)

  /**
    * Equality is symmetric and therefore can be flipped around.
    * Flipping is its own inverse, so `x.flip.flip == x`.
    */
  def flip: B === A =
    subst[? === A](refl)

  /**
    * Substitution on identity brings about a direct coercion function of the
    * same form that [[=:=]] provides.
    *
    * @see [[apply]]
    */
  def coerce(a: A): B =
    subst[λ[α => α]](a)

  def onF[X](fa: X => A): X => B =
    subst[X => ?](fa)

  def lift[F[_]]: F[A] === F[B] =
    Leibniz.lift(ab)

  def lift2[F[_, _]]: PartiallyAppliedLift2[F] =
    new PartiallyAppliedLift2[F]
  final class PartiallyAppliedLift2[F[_, _]] {
    def apply[I, J](ij: I === J): F[A, I] === F[B, J] =
      Leibniz.lift2(ab, ij)
  }

  /**
    * A value `Leibniz[A, B]` is always sufficient to produce a similar [[=:=]]
    * value.
    */
  def toPredef: A =:= B =
    subst[A =:= ?](implicitly[A =:= A])
}

object Leibniz {
  private[this] final case class Refl[A]() extends Leibniz[A, A] {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }
  private[data] val anyRefl: Any === Any = Refl[Any]()

  // FIXME: This optimization is safe:
  // def refl[A]: A === A = anyRefl.asInstanceOf[A === A]
  def refl[A]: A === A = Refl[A]()

  def lift[F[_], A, B]
  (ab: A === B): F[A] === F[B] =
    ab.subst[λ[α => F[A] === F[α]]](refl)

  def lift2[F[_, _], A, B, I, J]
  (ab: A === B, ij: I === J): F[A, I] === F[B, J] =
    ij.subst[λ[α => F[A, I] === F[B, α]]](
      ab.subst[λ[α => F[A, I] === F[α, I]]](
        refl[F[A, I]]))

  def lift3[F[_, _, _], A, B, I, J, M, N]
  (ab: A === B, ij: I === J, mn: M === N): F[A, I, M] === F[B, J, N] =
    mn.subst[λ[α => F[A, I, M] === F[B, J, α]]](
      ij.subst[λ[α => F[A, I, M] === F[B, α, M]]](
        ab.subst[λ[α => F[A, I, M] === F[α, I, M]]](
          refl[F[A, I, M]])))

  /**
    * It can be convenient to convert a [[=:=]] value into a `Leibniz` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A =:= B` implies `Leibniz[A, B]` it is not the case that you can create
    * evidence of `Leibniz[A, B]` except via a coercion. Use responsibly.
    */
  def unsafeFromPredef[A, B](eq: A =:= B): A === B =
    anyRefl.asInstanceOf[A === B]
}
