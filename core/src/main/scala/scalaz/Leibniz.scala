package scalaz


/** Leibnizian equality: A better =:= 
  *
  * This technique was first used in 
  * <a href="http://portal.acm.org/citation.cfm?id=583852.581494">Typing Dynamic Typing</a> (Baars and Swierstra, ICFP 2002).
  *
  * It is generalized here to handle subtyping so that it can be used with constrained type constructors
  */

trait Leibniz[L<:H,H>:L,A>:L<:H,B>:L<:H] {
  def subst[F[_>:L<:H]](p: F[A]) : F[B]
  final def *[
    L2<:H2,LT<:HT,
    H2>:L2,HT>:LT,
    +[_>:L<:H,_>:L2<:H2]>:LT<:HT,
    C>:L2<:H2,D>:L2<:H2
  ](
    that: Leibniz[L2,H2,C,D]
  ) : Leibniz[LT,HT,A+C,B+D] = Leibniz.lift2[L,L2,LT,H,H2,HT,+,A,B,C,D](this,that)
  final def andThen[C>:L<:H](that: Leibniz[L,H,B,C]) : Leibniz[L,H,A,C] = Leibniz.trans[L,H,A,B,C](that,this)
  final def compose[C>:L<:H](that: Leibniz[L,H,C,A]) : Leibniz[L,H,C,B] = Leibniz.trans[L,H,C,A,B](this,that)
  final def inverse : Leibniz[L,H,B,A] = Leibniz.symm(this)
}

object Leibniz {
  type ~[A,B] = Leibniz[Nothing,Any,A,B]

  trait PartialApplyLeibniz[L<:H,H>:L] {
    type Apply[A>:L<:H,B>:L<:H] = Leibniz[L,H,A,B]
  }

  /** Equality is reflexive -- we rely on subtyping to expand this type */
  implicit def refl[L<:H,H>:L,A>:L<:H] : Leibniz[L,H,A,A] = new Leibniz[L,H,A,A] {
    def subst[F[_>:L<:H]](p:F[A]): F[A]= p
  }

  /** We can witness equality by using it to convert between types 
   * We rely on subtyping to enable this to work for any Leibniz arrow 
   */
  implicit def witness[L<:H,H>:L,A>:L<:H,B>:L<:H](f: Leibniz[L,H,A,B]) : A => B = 
     f.subst[PartialApply1Of2[Function1,A]#Apply](identity)

  /** Equality is transitive */
  def trans[L<:H,H>:L,A>:L<:H,B>:L<:H,C>:L<:H](f: Leibniz[L,H,B,C], g: Leibniz[L,H,A,B]) : Leibniz[L,H,A,C] = {
    type f[X>:L<:H] = Leibniz[L,H,A,X]
    f.subst[f](g)
  }

  /** Equality is symmetric */
  def symm[L<:H,H>:L,A>:L<:H,B>:L<:H](f: Leibniz[L,H,A,B]) : Leibniz[L,H,B,A] = {
    type f[X>:L<:H] = Leibniz[L,H,X,A]
    f.subst[f](refl)
  }

  sealed class LeibnizGroupoid[L_ <: H_,H_ >: L_] extends GeneralizedGroupoid {
    type L = L_
    type H = H_
    type ~>[A>:L<:H,B>:L<:H] = Leibniz[L,H,A,B]

    def id[A>:L<:H] : Leibniz[L,H,A,A] = refl[L,H,A]

    def compose[A>:L<:H,B>:L<:H,C>:L<:H](
      bc: Leibniz[L,H,B,C],
      ab: Leibniz[L,H,A,B]
    ) : Leibniz[L,H,A,C] = trans[L,H,A,B,C](bc,ab)

    def invert[A>:L<:H,B>:L<:H](
      ab: Leibniz[L,H,A,B]
    ) : Leibniz[L,H,B,A] = symm(ab)
  }

  implicit def leibnizGroupoid[L<:H,H>:L] : LeibnizGroupoid[L,H] = new LeibnizGroupoid[L,H]

  /** We can lift equality into any type constructor */
  def lift[
    LA<:HA,LT<:HT,
    HA>:LA,HT>:LT,
    T[_>:LA<:HA]>:LT<:HT,
    A>:LA<:HA,A2>:LA<:HA
  ](
    a: Leibniz[LA,HA,A,A2]
  ) : Leibniz[LT,HT,T[A],T[A2]] = {
    type a[X>:LA<:HA] = Leibniz[LT,HT,T[A],T[X]]
    a.subst[a](refl)
  }

  /** We can lift equality into any type constructor */
  def lift2[
    LA<:HA,LB<:HB,LT<:HT,
    HA>:LA,HB>:LB,HT>:LT,
    T[_>:LA<:HA,_>:LB<:HB]>:LT<:HT,
    A>:LA<:HA,A2>:LA<:HA,
    B>:LB<:HB,B2>:LB<:HB
  ](
    a: Leibniz[LA,HA,A,A2],
    b: Leibniz[LB,HB,B,B2]
  ) : Leibniz[LT,HT,T[A,B],T[A2,B2]] = {
    type a[X>:LA<:HA] = Leibniz[LT,HT,T[A,B],T[X, B]]
    type b[X>:LB<:HB] = Leibniz[LT,HT,T[A,B],T[A2,X]]
    b.subst[b](a.subst[a](refl))
  }

  /** We can lift equality into any type constructor */
  def lift3[
   LA<:HA,LB<:HB,LC<:HC,LT<:HT,
   HA>:LA,HB>:LB,HC>:LC,HT>:LT,
   T[_>:LA<:HA,_>:LB<:HB,_>:LC<:HC]>:LT<:HT,
   A>:LA<:HA,A2>:LA<:HA,
   B>:LB<:HB,B2>:LB<:HB,
   C>:LC<:HC,C2>:LC<:HC
  ](
    a: Leibniz[LA,HA,A,A2],
    b: Leibniz[LB,HB,B,B2],
    c: Leibniz[LC,HC,C,C2]
  ) : Leibniz[LT,HT,T[A,B,C],T[A2,B2,C2]] = {
    type a[X>:LA<:HA] = Leibniz[LT,HT,T[A,B,C],T[X, B, C]]
    type b[X>:LB<:HB] = Leibniz[LT,HT,T[A,B,C],T[A2,X, C]]
    type c[X>:LC<:HC] = Leibniz[LT,HT,T[A,B,C],T[A2,B2,X]]
    c.subst[c](b.subst[b](a.subst[a](refl)))
  }


  /** 
   * Unsafe coercion between types. force abuses asInstanceOf to explicitly coerce types. 
   * It is unsafe, but needed where Leibnizian equality isn't sufficient 
   */

  def force[L<:H,H>:L,A>:L<:H,B>:L<:H] : Leibniz[L,H,A,B] = new Leibniz[L,H,A,B] {
    def subst[F[_>:L<:H]](fa: F[A]) : F[B] = fa.asInstanceOf[F[B]]
  }

  /**
   * Emir Pasalic's PhD thesis mentions that it is unknown whether or not <code>((A,B) ~ (C,D)) => (A ~ C)</code> is inhabited.
   * <p>
   * Haskell can work around this issue by abusing type families as noted in
   * <a href="http://osdir.com/ml/haskell-cafe@haskell.org/2010-05/msg00114.html">Leibniz equality can be injective</a> (Oleg Kiselyov, Haskell Cafe Mailing List 2010)
   * but we instead turn to force.
   * </p>
   *
   */

  import Injectivity._

  def lower[
    LA<:HA,HA>:LA,
    LT<:HT,HT>:LT,
    T[_>:LA<:HA] >:LT <:HT, //:Injective,
    A>:LA<:HA,A2>:LA<:HA
  ](
    t: Leibniz[LT,HT,T[A],T[A2]]
  ) : Leibniz[LA,HA,A,A2]
      = force[LA,HA,A,A2]

  def lower2[
    LA<:HA,HA>:LA,
    LB<:HB,HB>:LB,
    T[_>:LA<:HA,_>:LB<:HB], // :Injective2,
    A>:LA<:HA,A2>:LA<:HA,
    B>:LB<:HB,B2>:LB<:HB
  ](
    t: T[A,B] ~ T[A2,B2]
  ) : (Leibniz[LA,HA,A,A2], Leibniz[LB,HB,B,B2])
    = (force  [LA,HA,A,A2], force  [LB,HB,B,B2])

  /*
  implicit leibnizFunction1Functor 
    : GeneralizedFunctor[Id,~,Function1] = 
  new GeneralizedFunctor[Id,~,Function1] {
    def dom = implicitly[Category[~]]
    def cod = implicitly[Category[Function1]]
    def fmap[A,B](f: A ~ B): A => B = witness(f)
  }
  */
}
