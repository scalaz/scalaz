package scalaz

import Id._

/**
 * Leibnizian equality: a better `=:=`
 *
 * This technique was first used in
 * [[http://portal.acm.org/citation.cfm?id=583852.581494  Typing Dynamic Typing]] (Baars and Swierstra, ICFP 2002).
 *
 * It is generalized here to handle subtyping so that it can be used with constrained type constructors.
 *
 * `Leibniz[L,H,A,B]` says that `A` = `B`, and that both of its types are between `L` and `H`. Subtyping lets you
 * loosen the bounds on `L` and `H`.
 *
 * If you just need a witness that `A` = `B`, then you can use `A===B` which is a supertype of any `Leibniz[L,H,A,B]`
 *
 * The more refined types are useful if you need to be able to substitute into restricted contexts.
 */
sealed abstract class Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H] {
  def apply(a: A): B = subst[Id](a)
  def subst[F[_ >: L <: H]](p: F[A]): F[B]
  def compose[L2 <: L, H2 >: H, C >: L2 <: H2](that: Leibniz[L2, H2, C, A]): Leibniz[L2, H2, C, B] =
    Leibniz.trans[L2, H2, C, A, B](this, that)
  def andThen[L2 <: L, H2 >: H, C >: L2 <: H2](that: Leibniz[L2, H2, B, C]): Leibniz[L2, H2, A, C] =
    Leibniz.trans[L2, H2, A, B, C](that, this)

  def onF[X](fa: X => A): X => B = subst[X => ?](fa)
  def onCov[FA](fa: FA)(implicit U: Unapply.AuxA[Functor, FA, A]): U.M[B] =
    subst(U(fa))
  def onContra[FA](fa: FA)(implicit U: Unapply.AuxA[Contravariant, FA, A]): U.M[B] =
    subst(U(fa))
}

sealed abstract class LeibnizInstances {
  import Leibniz._

  implicit val leibniz: Category[===] = new Category[===] {
    def id[A]: (A === A) = refl[A]

    def compose[A, B, C](bc: B === C, ab: A === B) = bc compose ab
  }

  // TODO
  /*sealed class LeibnizGroupoid[L_, H_ >: L_] extends GeneralizedGroupoid with Hom {
      type L = L_
      type H = H_
      type C[A >: L <: H, B >: L <: H] = Leibniz[L, H, A, B]
      type U = LeibnizGroupoid[L, H]

      def id[A >: L <: H]: Leibniz[A, A, A, A] = refl[A]

      def compose[A >: L <: H, B >: L <: H, C >: L <: H](
        bc: Leibniz[L, H, B, C],
        ab: Leibniz[L, H, A, B]
      ): Leibniz[L, H, A, C] = trans[L, H, A, B, C](bc, ab)

      def invert[A >: L <: H, B >: L <: H](
        ab: Leibniz[L, H, A, B]
      ): Leibniz[L, H, B, A] = symm(ab)
    }

    implicit def leibnizGroupoid[L, H >: L]: LeibnizGroupoid[L, H] = new LeibnizGroupoid[L, H]*/

}

object Leibniz extends LeibnizInstances {

  /** `(A === B)` is a supertype of `Leibniz[L,H,A,B]` */
  type ===[A,B] = Leibniz[⊥, ⊤, A, B]

  /** Equality is reflexive -- we rely on subtyping to expand this type */
  implicit def refl[A]: Leibniz[A, A, A, A] = new Leibniz[A, A, A, A] {
    def subst[F[_ >: A <: A]](p: F[A]): F[A] = p
  }

  /** We can witness equality by using it to convert between types
   * We rely on subtyping to enable this to work for any Leibniz arrow
   */
  implicit def witness[A, B](f: A === B): A => B =
    f.subst[A => ?](identity)

  implicit def subst[A, B](a: A)(implicit f: A === B): B = f.subst[Id](a)

  /** Equality is transitive */
  def trans[L, H >: L, A >: L <: H, B >: L <: H, C >: L <: H](
    f: Leibniz[L, H, B, C],
    g: Leibniz[L, H, A, B]
  ): Leibniz[L, H, A, C] =
    f.subst[λ[`X >: L <: H` => Leibniz[L, H, A, X]]](g) // note kind-projector 0.5.2 cannot do super/subtype bounds

  /** Equality is symmetric */
  def symm[L, H >: L, A >: L <: H, B >: L <: H](
    f: Leibniz[L, H, A, B]
  )  : Leibniz[L, H, B, A] =
    f.subst[λ[`X>:L<:H` => Leibniz[L, H, X, A]]](refl)

  /** We can lift equality into any type constructor */
  def lift[
    LA, LT,
    HA >: LA, HT >: LT,
    T[_ >: LA <: HA] >: LT <: HT,
    A >: LA <: HA, A2 >: LA <: HA
  ](
    a: Leibniz[LA, HA, A, A2]
  ): Leibniz[LT, HT, T[A], T[A2]] =
    a.subst[λ[`X >: LA <: HA` => Leibniz[LT, HT, T[A], T[X]]]](refl)

  /** We can lift equality into any type constructor */
  def lift2[
    LA, LB, LT,
    HA >: LA, HB >: LB, HT >: LT,
    T[_ >: LA <: HA, _ >: LB <: HB] >: LT <: HT,
    A >: LA <: HA, A2 >: LA <: HA,
    B >: LB <: HB, B2 >: LB <: HB
  ](
    a: Leibniz[LA, HA, A, A2],
    b: Leibniz[LB, HB, B, B2]
  ) : Leibniz[LT, HT, T[A, B], T[A2, B2]] =
    b.subst[λ[`X >: LB <: HB` => Leibniz[LT, HT, T[A, B], T[A2, X]]]](
      a.subst[λ[`X >: LA <: HA` => Leibniz[LT, HT, T[A, B], T[X, B]]]](
        refl))

  /** We can lift equality into any type constructor */
  def lift3[
    LA, LB, LC, LT,
    HA >: LA, HB >: LB, HC >: LC, HT >: LT,
    T[_ >: LA <: HA, _ >: LB <: HB, _ >: LC <: HC] >: LT <: HT,
    A >: LA <: HA, A2 >: LA <: HA,
    B >: LB <: HB, B2 >: LB <: HB,
    C >: LC <: HC, C2 >: LC <: HC
  ](
    a: Leibniz[LA, HA, A, A2],
    b: Leibniz[LB, HB, B, B2],
    c: Leibniz[LC, HC, C, C2]
  ): Leibniz[LT, HT, T[A, B, C], T[A2, B2, C2]] =
    c.subst[λ[`X >: LC <: HC` => Leibniz[LT, HT, T[A, B, C], T[A2, B2, X]]]](
      b.subst[λ[`X >: LB <: HB` => Leibniz[LT, HT, T[A, B, C], T[A2, X, C]]]](
        a.subst[λ[`X >: LA <: HA` => Leibniz[LT, HT, T[A, B, C], T[X, B, C]]]](
          refl)))

  /**
   * Unsafe coercion between types. force abuses asInstanceOf to explicitly coerce types.
   * It is unsafe, but needed where Leibnizian equality isn't sufficient
   */
  def force[L, H >: L, A >: L <: H, B >: L <: H]: Leibniz[L, H, A, B] = new Leibniz[L, H, A, B] {
    def subst[F[_ >: L <: H]](fa: F[A]): F[B] = fa.asInstanceOf[F[B]]
  }


  /**
   * Emir Pasalic's PhD thesis mentions that it is unknown whether or not `((A,B) === (C,D)) => (A === C)` is inhabited.
   * <p>
   * Haskell can work around this issue by abusing type families as noted in
   * <a href="http://osdir.com/ml/haskell-cafe@haskell.org/2010-05/msg00114.html">Leibniz equality can be injective</a> (Oleg Kiselyov, Haskell Cafe Mailing List 2010)
   * but we instead turn to force.
   * </p>
   *
   */
  def lower[
    LA, HA >: LA,
    T[_ >: LA <: HA] /*: Injective*/,
    A >: LA <: HA, A2 >: LA <: HA
  ](
    t: T[A] === T[A2]
  ): Leibniz[LA, HA, A, A2] = force[LA, HA, A, A2]

  def lower2[
    LA, HA >: LA,
    LB, HB >: LB,
    T[_ >: LA <: HA, _ >: LB <: HB]/*: Injective2*/,
    A >: LA <: HA, A2 >: LA <: HA,
    B >: LB <: HB, B2 >: LB <: HB
  ](
   t: T[A, B] === T[A2, B2]
  ): (Leibniz[LA, HA, A, A2], Leibniz[LB, HB, B, B2]) = (force[LA, HA, A, A2], force[LB, HB, B, B2])
}
