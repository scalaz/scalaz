package scalaz
package data

import Prelude.{ ===, <~<, =~= }

/**
  * This particular version of Leibnitz’ equality has been generalized to
  * handle subtyping so that it can be used with constrained type constructors,
  * such as `F[_ >: L <: H]. `Leibniz[L, H, A, B]` witnesses both `A === B` and
  * that [[A]] and [[B]] are between [[L]] and [[H]]. Subtyping lets you loosen
  * the bounds on [[L]] and [[H]].
  *
  * @see [[Is]]
  */
sealed abstract class Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H] private[Leibniz]() { ab =>
  import Leibniz._

  /**
    * To create an instance of `Leibniz[L, H, A, B]` you must show that for
    * every choice of `F[_ >: L <: H]` you can convert `F[A]` to `F[B]`.
    */
  def subst[F[_ >: L <: H]](fa: F[A]): F[B]

  /**
    * Substitution on identity brings about a direct coercion function of the
    * same form that [[=:=]] provides.
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
    type f[a >: L <: H] = a
    subst[f](a)
  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[compose]]
    */
  final def andThen[L2 <: L, H2 >: H, C >: L2 <: H2]
  (bc: Leibniz[L2, H2, B, C]): Leibniz[L2, H2, A, C] =
    Leibniz.trans[L2, H2, A, B, C](bc, ab)

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[L2 <: L, H2 >: H, Z >: L2 <: H2]
  (za: Leibniz[L2, H2, Z, A]): Leibniz[L2, H2, Z, B] =
    Leibniz.trans[L2, H2, Z, A, B](ab, za)

  /**
    * Equality is symmetric relation and therefore can be flipped around.
    * Flipping is its own inverse, so `x.flip.flip == x`.
    */
  final def flip: Leibniz[L, H, B, A] =
    Leibniz.symm(ab)

  /**
    * Given `Leibniz[L, H, A, B]` we can prove `Leibniz[LF, HF, F[A], F[B]` for
    * any `F[X >: L <: H] >: LF <: HF`.
    *
    * @see [[Leibniz.lift]]
    * @see [[Leibniz.lift2]]
    * @see [[Leibniz.lift3]]
    */
  final def lift[LF, HF >: LF, F[_ >: L <: H] >: LF <: HF]: Leibniz[LF, HF, F[A], F[B]] =
    Leibniz.lift[L, H, A, B, LF, HF, F](ab)

  /**
    * Given `Liskov[L, H, A, B]` we can convert `(X => A)` into `(X => B)`.
    */
  final def onF[X](fa: X => A): X => B = {
    type f[a] = X => a
    subst[f](fa)
  }

  /**
    * Given `Leibniz[L, H, A, B]`, prove `A =:= B`.
    */
  final def toPredef: A =:= B = {
    type f[a >: L <: H] = A =:= a
    subst[f](implicitly[A =:= A])
  }

  /**
    * Given `Leibniz[L, H, A, B]`, prove `A === B`.
    */
  final def toIs: A === B = {
    type f[a >: L <: H] = A === a
    subst[f](Is.refl[A])
  }

  /**
    * Given `Leibniz[L, H, A, B]`, prove `A <~< B`.
    */
  final def toAs: A <~< B = {
    type f[a >: L <: H] = A <~< a
    subst[f](As.refl[A])
  }

  /**
    * Given `Leibniz[L, H, A, B]`, prove `Liskov[L, H, A, B]`.
    */
  final def toLiskov: Liskov[L, H, A, B] = {
    type f[a >: L <: H] = Liskov[L, H, A, a]
    subst[f](Liskov.refl[A])
  }
}

object Leibniz {
  def apply[L, H >: L, A >: L <: H, B >: L <: H]
  (implicit ab: Leibniz[L, H, A, B]): Leibniz[L, H, A, B] = ab

  final case class Refl[A]() extends Leibniz[A, A, A, A] {
    def subst[F[_ >: A <: A]](fa: F[A]): F[A] = fa
  }
  private[this] val anyRefl: Leibniz[Nothing, Any, Any, Any] = Refl[Any]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe, but needed where Leibnizian
    * equality isn't sufficient.
    */
  def unsafeForce[L, H >: L, A >: L <: H, B >: L <: H]: Leibniz[L, H, A, B] =
  anyRefl.asInstanceOf[Leibniz[L, H, A, B]]

  final def bound[L, H >: L, A >: L <: H, B >: L <: H]
  (ab: Is[A, B]): Leibniz[L, H, A, B] =
    unsafeForce[L, H, A, B]

  /**
    * Equality is reflexive relation.
    */
  implicit def refl[A]: Leibniz[A, A, A, A] =
    unsafeForce[A, A, A, A]

  /**
    * Equality is reflexive relation. Compared to [[refl]], this function
    * can be used to specify bounds up front instead of relying on subtyping.
    */
  def refl_[L, H >: L, A >: L <: H]: Leibniz[L, H, A, A] =
    unsafeForce[L, H, A, A]

  /**
    * Substitution on identity brings about a direct coercion function of the
    * same form that `=:=` provides.
    *
    * @see [[Leibniz.apply]]
    */
  def coerce[L, H >: L, A >: L <: H, B >: L <: H]
  (a: A, ab: Leibniz[L, H, A, B]): B =
    ab.subst[λ[α => α]](a)

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[Leibniz.compose]]
    * @see [[Leibniz.andThen]]
    */
  def trans[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H]
  (bc: Leibniz[L, H, B, C], ab: Leibniz[L, H, A, B]): Leibniz[L, H, A, C] =
    bc.subst[λ[`α >: L <: H` => Leibniz[L, H, A, α]]](ab)

  /**
    * Equality is symmetric and therefore can be flipped around.
    *
    * @see [[Leibniz.flip]]
    */
  def symm[L, H >: L, A >: L <: H, B >: L <: H]
  (ab: Leibniz[L, H, A, B]): Leibniz[L, H, B, A] =
    ab.subst[λ[`α >: L <: H` => Leibniz[L, H, α, A]]](refl)

  /**
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[lift2]]
    * @see [[lift3]]
    */
  def lift[
    L, H >: L, A >: L <: H, B >: L <: H,
    LF, HF >: LF, F[_ >: L <: H] >: LF <: HF
  ] (
    eq: Leibniz[L, H, A, B]
  ): Leibniz[LF, HF, F[A], F[B]] = {
    type f[α >: L <: H] = Leibniz[LF, HF, F[A], F[α]]
    eq.subst[f](refl_[LF, HF, F[A]])
  }

  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def pair[
    L1, H1 >: L1, A1 >: L1 <: H1, B1 >: L1 <: H1,
    L2, H2 >: L2, A2 >: L2 <: H2, B2 >: L2 <: H2
  ] (
    eq1: Leibniz[L1, H1, A1, B1],
    eq2: Leibniz[L2, H2, A2, B2]
  ) : Pair[L1, H1, A1, B1, L2, H2, A2, B2] = Pair(eq1, eq2)

  final case class Pair[
    L1, H1 >: L1, A1 >: L1 <: H1, B1 >: L1 <: H1,
    L2, H2 >: L2, A2 >: L2 <: H2, B2 >: L2 <: H2
  ](eq1: Leibniz[L1, H1, A1, B1], eq2: Leibniz[L2, H2, A2, B2]) {
    def bounded[
      LF, HF >: LF, F[_ >: L1 <: H1, _ >: L2 <: H2] >: LF <: HF
    ]: Leibniz[LF, HF, F[A1, A2], F[B1, B2]] = {
      type f1[a1 >: L1 <: H1] = Leibniz[LF, HF, F[A1, A2], F[a1, A2]]
      type f2[a2 >: L2 <: H2] = Leibniz[LF, HF, F[A1, A2], F[B1, a2]]
      eq2.subst[f2](eq1.subst[f1](refl_[LF, HF, F[A1, A2]]))
    }

    def subst[F[_ >: L1 <: H1, _ >: L2 <: H2]](f1: F[A1, A2]): F[B1, B2] = {
      type f1[a1 >: L1 <: H1] = F[a1, A2]
      type f2[a2 >: L2 <: H2] = F[B1, a2]
      eq2.subst[f2](eq1.subst[f1](f1))
    }

    def unbounded[F[_ >: L1 <: H1, _ >: L2 <: H2]]: F[A1, A2] === F[B1, B2] = {
      type f1[a1 >: L1 <: H1] = F[A1, A2] Is F[a1, A2]
      type f2[a2 >: L2 <: H2] = F[A1, A2] Is F[B1, a2]
      eq2.subst[f2](eq1.subst[f1](Is.refl[F[A1, A2]]))
    }
  }

  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def lift2[
    L1, H1 >: L1, A1 >: L1 <: H1, B1 >: L1 <: H1,
    L2, H2 >: L2, A2 >: L2 <: H2, B2 >: L2 <: H2,
    LF, HF >: LF, F[_ >: L1 <: H1, _ >: L2 <: H2] >: LF <: HF
  ] (
      eq1: Leibniz[L1, H1, A1, B1],
      eq2: Leibniz[L2, H2, A2, B2]
  ): Leibniz[LF, HF, F[A1, A2], F[B1, B2]] = {
    type f1[a1 >: L1 <: H1] = Leibniz[LF, HF, F[A1, A2], F[a1, A2]]
    type f2[a2 >: L2 <: H2] = Leibniz[LF, HF, F[A1, A2], F[B1, a2]]
    eq2.subst[f2](eq1.subst[f1](refl_[LF, HF, F[A1, A2]]))
  }

  /**
    * Given `A === B`, `I === J`, and `M === N` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lift]]
    * @see [[lift2]]
    */
  def lift3[
    L1, H1 >: L1, A1 >: L1 <: H1, B1 >: L1 <: H1,
    L2, H2 >: L2, A2 >: L2 <: H2, B2 >: L2 <: H2,
    L3, H3 >: L3, A3 >: L3 <: H3, B3 >: L3 <: H3,
    LF, HF >: LF, F[_ >: L1 <: H1, _ >: L2 <: H2, _ >: L3 <: H3] >: LF <: HF
  ] (
      eq1: Leibniz[L1, H1, A1, B1],
      eq2: Leibniz[L2, H2, A2, B2],
      eq3: Leibniz[L3, H3, A3, B3]
  ): Leibniz[LF, HF, F[A1, A2, A3], F[B1, B2, B3]] = {
    type f1[a1 >: L1 <: H1] = Leibniz[LF, HF, F[A1, A2, A3], F[a1, A2, A3]]
    type f2[a2 >: L2 <: H2] = Leibniz[LF, HF, F[A1, A2, A3], F[B1, a2, A3]]
    type f3[a3 >: L3 <: H3] = Leibniz[LF, HF, F[A1, A2, A3], F[B1, B2, a3]]
    eq3.subst[f3](eq2.subst[f2](eq1.subst[f1](refl_[LF, HF, F[A1, A2, A3]])))
  }

  /**
    * It can be convenient to convert a [[=:=]] value into a `Leibniz` value.
    */
  def fromPredef[L, H >: L, A >: L <: H, B >: L <: H]
  (eq: A =:= B): Leibniz[L, H, A, B] =
    unsafeForce[L, H, A, B]

  /**
    * It can be convenient to convert a [[===]] value into a `Leibniz` value.
    */
  def fromIs[L, H >: L, A >: L <: H, B >: L <: H]
  (eq: A === B): Leibniz[L, H, A, B] =
    unsafeForce[L, H, A, B]
}
