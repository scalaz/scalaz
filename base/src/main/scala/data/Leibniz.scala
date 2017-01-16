package scalaz
package data

import Prelude.===
import Leibniz.refl

/**
  * The data type `Leibniz` is the encoding of Leibnitz’ law which states that
  * if `a` and `b` are identical then they must have identical properties.
  * Leibnitz’ original definition reads as follows:
  *   a ≡ b = ∀ f .f a ⇔ f b
  * and can be proven to be equivalent to:
  *   a ≡ b = ∀ f .f a → f b
  *
  * The `Leibniz` data type encodes true type equality, since the identity
  * function is the only non-diverging conversion function that can be used
  * as an implementation of the `subst` method assuming that we do not break
  * parametricity. As the substitution function has to work for any `F[_]`, it
  * cannot make assumptions about the structure of `F[_]`, making it impossible
  * to construct a value of type `F[A]` or to access values of type `A` that
  * may be stored inside a value of type `F[A]`. Hence it is impossible for
  * a substitution function to alter the value it takes as argument.
  *
  * Not taking into account the partial functions that never terminate
  * (infinite loops), functions returning `null`, or throwing exceptions,
  * the identity function is the only function that can be used in place of
  * `subst` to construct a value of type `Leibniz[A, B]`.
  *
  * The existence of a value of type `Leibniz[A, B]` now implies that a ≡ b,
  * since the conversion function, that converts an `A` into a `B`, must be
  * the identity function.
  *
  * This technique was first used in
  * [[http://portal.acm.org/citation.cfm?id=583852.581494
  * Typing Dynamic Typing]] (Baars and Swierstra, ICFP 2002).
  *
  * @see [[===]] `A === B` is a type synonym to `Leibniz[A, B]`
  * @see [[http://typelevel.org/blog/2014/09/20/higher_leibniz.html
  *        Higher Leibniz]]
  */
sealed abstract class Leibniz[A, B] private[Leibniz] () { ab =>
  /**
    * To create an instance of `Leibniz[A, B]` you must show that for every
    * choice of `F[_]` you can convert `F[A]` to `F[B]`.
    */
  def subst[F[_]](fa: F[A]): F[B]

  /**
    * Substitution on identity brings about a direct coercion function of the
    * same form that `=:=` provides.
    *
    * @see [[coerce]]
    */
  final def apply(a: A): B =
    coerce(a)

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[compose]]
    */
  final def andThen[C](bc: B === C): A === C =
    bc.subst[A === ?](ab)

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[Z](za: Z === A): Z === B =
    za.andThen(ab)

  /**
    * Equality is symmetric relation and therefore can be flipped around.
    * Flipping is its own inverse, so `x.flip.flip == x`.
    */
  final def flip: B === A =
    subst[? === A](refl)

  /**
    * Substitution on identity brings about a direct coercion function of the
    * same form that [[=:=]] provides.
    *
    * @see [[apply]]
    */
  final def coerce(a: A): B =
    subst[λ[α => α]](a)

  final def onF[X](fa: X => A): X => B =
    subst[X => ?](fa)

  /**
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[Leibniz.lift]]
    * @see [[Leibniz.lift2]]
    */
  final def lift[F[_]]: F[A] === F[B] =
    Leibniz.lift(ab)

  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * This method allows you to compose two `Leibniz` values in an infix
    * manner:
    * {{{
    *   def either(ab: A === B, ij: I === J): Either[A, I] === Either[B, J] =
    *     ab lift2[Either] ij
    * }}}
    *
    * @see [[Leibniz.lift]]
    * @see [[Leibniz.lift2]]
    */
  final def lift2[F[_, _]]: PartiallyAppliedLift2[F] =
    new PartiallyAppliedLift2[F]
  final class PartiallyAppliedLift2[F[_, _]] {
    def apply[I, J](ij: I === J): F[A, I] === F[B, J] =
      Leibniz.lift2(ab, ij)
  }

  /**
    * A value `Leibniz[A, B]` is always sufficient to produce a similar [[=:=]]
    * value.
    */
  final def toPredef: A =:= B =
    subst[A =:= ?](implicitly[A =:= A])
}

object Leibniz {
  private[this] final case class Refl[A]() extends Leibniz[A, A] {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }
  private[data] val anyRefl: Any === Any = Refl[Any]()

  /**
    * Equality is reflexive relation.
    */
  def refl[A]: A === A = {
    // FIXME: This optimization is safe:
    // anyRefl.asInstanceOf[A === A]
    Refl[A]()
  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[Leibniz.compose]]
    * @see [[Leibniz.andThen]]
    */
  def trans[A, B, C](bc: B === C, ab: A === B): A === C =
    ab.andThen(bc)

  /**
    * Equality is symmetric and therefore can be flipped around.
    *
    * @see [[Leibniz.flip]]
    */
  def symm[A, B](ab: A === B): B === A =
    ab.flip

  /**
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[lift2]]
    * @see [[lift3]]
    */
  def lift[F[_], A, B]
  (ab: A === B): F[A] === F[B] =
    ab.subst[λ[α => F[A] === F[α]]](refl)

  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def lift2[F[_, _], A, B, I, J]
  (ab: A === B, ij: I === J): F[A, I] === F[B, J] =
    ij.subst[λ[α => F[A, I] === F[B, α]]](
      ab.subst[λ[α => F[A, I] === F[α, I]]](
        refl[F[A, I]]))

  /**
    * Given `A === B`, `I === J`, and `M === N` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift2]]
    */
  def lift3[F[_, _, _], A, B, I, J, M, N]
  (ab: A === B, ij: I === J, mn: M === N): F[A, I, M] === F[B, J, N] =
    mn.subst[λ[α => F[A, I, M] === F[B, J, α]]](
      ij.subst[λ[α => F[A, I, M] === F[B, α, M]]](
        ab.subst[λ[α => F[A, I, M] === F[α, I, M]]](
          refl[F[A, I, M]])))

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe, but needed where Leibnizian
    * equality isn't sufficient.
    */
  def unsafeForce[A, B]: A === B =
    anyRefl.asInstanceOf[A === B]

  /**
    * It can be convenient to convert a [[=:=]] value into a `Leibniz` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A =:= B` implies `A === B` it is not the case that you can create
    * evidence of `A === B` except via a coercion. Use responsibly.
    */
  def unsafeFromPredef[A, B](eq: A =:= B): A === B =
    unsafeForce[A, B]
}
