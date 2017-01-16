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
  * `subst` to construct a value of type `Leibniz[L, H, A, B]`.
  *
  * This particular version of Leibnitz’ equality has been generalized to
  * handle subtyping so that it can be used with constrained type constructors
  * such as `F[_ <: AnyRef]`. `Leibniz[L, H, A, B]` says that `A` = `B`, and
  * that both of them are between `L` and `H`. Subtyping lets you loosen
  * the bounds on `L` and `H`.
  *
  * This technique was first used in
  * [[http://portal.acm.org/citation.cfm?id=583852.581494
  * Typing Dynamic Typing]] (Baars and Swierstra, ICFP 2002).
  *
  * @see [[===]] `A === B` is a type synonym to `Leibniz[A, B]`
  * @see [[http://typelevel.org/blog/2014/09/20/higher_leibniz.html
  *        Higher Leibniz]]
  */
sealed abstract class Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H] private[Leibniz] () { ab =>
  /**
    * To create an instance of `Leibniz[A, B]` you must show that for every
    * choice of `F[_]` you can convert `F[A]` to `F[B]`.
    */
  def subst[F[_ >: L <: H]](fa: F[A]): F[B]

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
  final def coerce(a: A): B =
    subst[λ[α => α]](a)

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
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[Leibniz.lift]]
    * @see [[Leibniz.lift2]]
    * @see [[Leibniz.lift3]]
    */
  final def lift[LF, HF >: LF, F[_ >: L <: H] >: LF <: HF]: Leibniz[LF, HF, F[A], F[B]] =
    Leibniz.lift[L, H, A, B, LF, HF, F](ab)

  final def onF[X](fa: X => A): X => B =
    subst[X => ?](fa)

  /**
    * A value `Leibniz[A, B]` is always sufficient to produce a similar [[=:=]]
    * value.
    */
  final def toPredef: A =:= B =
    subst[A =:= ?](implicitly[A =:= A])
}

object Leibniz {
  private[this] final case class Refl[A]() extends Leibniz[A, A, A, A] {
    def subst[F[_ >: A <: A]](fa: F[A]): F[A] = fa
  }
  private[data] val anyRefl: Leibniz[Nothing, Any, Any, Any] = Refl[Any]()

  /**
    * Equality is reflexive relation.
    */
  def refl[A]: Leibniz[A, A, A, A] = {
    // This optimization is safe because of erasure.
    anyRefl.asInstanceOf[Leibniz[A, A, A, A]]
  }

  /**
    * Equality is reflexive relation. Compared to [[refl]], this function
    * can be used to specify bounds up front instead of relying on subtyping.
    */
  def refl_[L, H >: L, A >: L <: H]: Leibniz[L, H, A, A] = {
    // This optimization is safe because of erasure.
    anyRefl.asInstanceOf[Leibniz[L, H, A, A]]
  }

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
  def lift2[
    L1, H1 >: L1, A1 >: L1 <: H1, B1 >: L1 <: H1,
    L2, H2 >: L2, A2 >: L2 <: H2, B2 >: L2 <: H2,
    LF, HF >: LF, F[_ >: L1 <: H1, _ >: L2 <: H2] >: LF <: HF
  ] (
    eq1: Leibniz[L1, H1, A1, B1],
    eq2: Leibniz[L2, H2, A2, B2]
  ): Leibniz[LF, HF, F[A1, A2], F[B1, B2]] = {
    type f1[α >: L1 <: H1] = Leibniz[LF, HF, F[A1, A2], F[α, A2]]
    type f2[α >: L2 <: H2] = Leibniz[LF, HF, F[A1, A2], F[B1, α]]
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
    type f1[α >: L1 <: H1] = Leibniz[LF, HF, F[A1, A2, A3], F[α, A2, A3]]
    type f2[α >: L2 <: H2] = Leibniz[LF, HF, F[A1, A2, A3], F[B1, α, A3]]
    type f3[α >: L3 <: H3] = Leibniz[LF, HF, F[A1, A2, A3], F[B1, B2, α]]
    eq3.subst[f3](eq2.subst[f2](eq1.subst[f1](refl_[LF, HF, F[A1, A2, A3]])))
  }

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
