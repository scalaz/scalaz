package scalaz
package data

import Prelude.{===, <~<}

/**
  * The data type `Is` is the encoding of Leibnitz’ law which states that
  * if `a` and `b` are identical then they must have identical properties.
  * Leibnitz’ original definition reads as follows:
  *   a ≡ b = ∀ f .f a ⇔ f b
  * and can be proven to be equivalent to:
  *   a ≡ b = ∀ f .f a → f b
  *
  * The `Is` data type encodes true type equality, since the identity
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
  * `subst` to construct a value of type `Is[A, B]`.
  *
  * The existence of a value of type `Is[A, B]` now implies that a ≡ b,
  * since the conversion function, that converts an `A` into a `B`, must be
  * the identity function.
  *
  * This technique was first used in
  * [[http://portal.acm.org/citation.cfm?id=583852.581494
  * Typing Dynamic Typing]] (Baars and Swierstra, ICFP 2002).
  *
  * @see [[===]] `A === B` is a type synonym to `Is[A, B]`
  * @see [[http://typelevel.org/blog/2014/09/20/higher_leibniz.html
  *        Higher Leibniz]]
  */
sealed abstract class Is[A, B] private[Is]()  { ab =>
  import Is._

  /**
    * To create an instance of `Is[A, B]` you must show that for every
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
    * Substitution on identity brings about a direct coercion function of the
    * same form that [[=:=]] provides.
    *
    * @see [[apply]]
    */
  final def coerce(a: A): B = {
    type f[a] = a
    subst[f](a)
  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[compose]]
    */
  final def andThen[C](bc: B === C): A === C = {
    type f[b] = A === b
    bc.subst[f](ab)
  }

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
  final def flip: B === A = {
    type f[a] = a === A
    subst[f](refl)
  }


  /**
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[Is.lift]]
    * @see [[Is.lift2]]
    */
  final def lift[F[_]]: F[A] === F[B] =
    Is.lift(ab)

  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * This method allows you to compose two `===` values in infix manner:
    * {{{
    *   def either(ab: A === B, ij: I === J): Either[A, I] === Either[B, J] =
    *     ab lift2[Either] ij
    * }}}
    *
    * @see [[Is.lift]]
    * @see [[Is.lift2]]
    * @see [[Is.lift3]]
    */
  def lift2[F[_, _]]: PartiallyAppliedLift2[F] =
    new PartiallyAppliedLift2[F]
  final class PartiallyAppliedLift2[F[_, _]] {
    def apply[I, J](ij: I === J): F[A, I] === F[B, J] =
      Is.lift2(ab, ij)
  }

  /**
    * Given `A === B` we can convert `(X => A)` into `(X => B)`.
    */
  def onF[X](fa: X => A): X => B = {
    type f[a] = X => a
    subst[f](fa)
  }

  /**
    * A value `A === B` is always sufficient to produce a similar [[=:=]]
    * value.
    */
  def toPredef: A =:= B = {
    type f[a] = A =:= a
    subst[f](implicitly[A =:= A])
  }

  /**
    * A value `A === B` is always sufficient to produce a `A <~< B`
    * value.
    */
  def toAs: A <~< B = {
    type f[a] = A <~< a
    subst[f](As.refl[A])
  }
}

object Is {
  def apply[A, B](implicit ev: A Is B): A Is B = ev

  final case class Refl[A]() extends Is[A, A] {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }

  private[this] val anyRefl: Any === Any = Refl[Any]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe, but needed where Leibnizian
    * equality isn't sufficient.
    */
  def unsafeForce[A, B]: A === B =
    anyRefl.asInstanceOf[A === B]

  /**
    * Equality is reflexive relation.
    */
  implicit def refl[A]: A === A =
    unsafeForce[A, A]

  /**
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[lift2]]
    * @see [[lift3]]
    */
  def lift[F[_], A, B]
  (ab: A === B): F[A] === F[B] = {
    type f[a] = F[A] === F[a]
    ab.subst[f](refl)
  }


  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def lift2[F[_, _], A, B, I, J]
  (ab: A === B, ij: I === J): F[A, I] === F[B, J] = {
    type f1[a] = F[A, I] === F[a, I]
    type f2[i] = F[A, I] === F[B, i]
    ij.subst[f2](ab.subst[f1](refl))
  }

  /**
    * Given `A === B`, `I === J`, and `M === N` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift2]]
    */
  def lift3[F[_, _, _], A, B, I, J, M, N]
  (ab: A === B, ij: I === J, mn: M === N): F[A, I, M] === F[B, J, N] = {
    type f1[a] = F[A, I, M] === F[a, I, M]
    type f2[i] = F[A, I, M] === F[B, i, M]
    type f3[m] = F[A, I, M] === F[B, J, m]
    mn.subst[f3](ij.subst[f2](ab.subst[f1](refl)))
  }

  /**
    * It can be convenient to convert a [[=:=]] value into a `Leibniz` value.
    * This is not strictly valid as while it is almost certainly true that
    * `A =:= B` implies `A === B` it is not the case that you can create
    * evidence of `A === B` except via a coercion. Use responsibly.
    */
  def fromPredef[A, B](eq: A =:= B): A === B =
    unsafeForce[A, B]
}