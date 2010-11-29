package scalaz

/** Leibnizian equality: A better =:= 
  *
  * This technique was first used in "Typing Dynamic Typing" (Baars and Swierstra, ICFP 2002) 
  * http://portal.acm.org/citation.cfm?id=583852.581494
  */

trait Leibniz[A,B] {
  def subst[F[_]](p: F[A]) : F[B]

  final def *[+[_,_],C,D](that: Leibniz[C,D]) : Leibniz[A+C,B+D] = Leibniz.lift2(this,that)
  final def andThen[C](that: Leibniz[B,C]) : Leibniz[A,C] = Leibniz.trans(that,this)
  final def compose[C](that: Leibniz[C,A]) : Leibniz[C,B] = Leibniz.trans(this,that)
  final def inverse : Leibniz[B,A] = Leibniz.symm(this)
}

object Leibniz { 

  /** A convenient type alias for Leibniz */
  type ~[A,B] = Leibniz[A,B] 

  /** We can witness equality by using it to convert between types */
  implicit def witness[A,B](f: A ~ B) : A => B = 
     f.subst[PartialApply1Of2[Function1,A]#Apply](identity)

  /** Equality is reflexive */
  implicit def refl[A] : (A ~ A) = new (A ~ A) {
    def subst[F[_]](p:F[A]): F[A]= p
  }

  /** Equality is transitive */
  def trans[A,B,C](f: B ~ C, g: A ~ B) = f.subst[PartialApply1Of2[~,A]#Apply](g)
 
  /** Equality is symmetric */
  def symm[A,B](f: A ~ B) : B ~ A = f.subst[PartialApply1Of2[~,A]#Flip](refl)

  /** Equivalence forms a category */
  implicit def LeibnizCategory: Category[~] = new Category[~] {
    def id[A] : (A ~ A) = refl[A]
    def compose[A,B,C](bc: B ~ C, ab: A ~ B) : (A ~ C) = trans(bc,ab)
  }

  /** We can lift equality into any type constructor */
  def lift[T[_],A,A2](
    a: A ~ A2
  ) : (T[A] ~ T[A2]) = {
    type a[X] = T[A] ~ T[X]
    a.subst[a](refl)
  }

  /** We can lift equality into any type constructor */
  def lift2[T[_,_],A,A2,B,B2](
    a: A ~ A2, b: B ~ B2
  ) : (T[A,B] ~ T[A2,B2]) = {
    type a[X] = T[A,B] ~ T[X, B]
    type b[X] = T[A,B] ~ T[A2,X]
    b.subst[b](a.subst[a](refl))
  }

  /** We can lift equality into any type constructor */
  def lift3[T[_,_,_],A,A2,B,B2,C,C2](
    a: A ~ A2, b: B ~ B2, c: C ~ C2
  ) : (T[A,B,C] ~ T[A2,B2,C2]) = {
    type a[X] = T[A,B,C] ~ T[X ,B, C]
    type b[X] = T[A,B,C] ~ T[A2,X, C]
    type c[X] = T[A,B,C] ~ T[A2,B2,X]
    c.subst[c](b.subst[b](a.subst[a](refl)))
  }

  /** We can lift equality into any type constructor */
  def lift4[T[_,_,_,_],A,A2,B,B2,C,C2,D,D2](
    a: A ~ A2, b: B ~ B2, c: C ~ C2, d: D ~ D2
  ) : (T[A,B,C,D] ~ T[A2,B2,C2,D2]) = {
    type a[X] = T[A,B,C,D] ~ T[X, B, C, D]
    type b[X] = T[A,B,C,D] ~ T[A2,X, C, D]
    type c[X] = T[A,B,C,D] ~ T[A2,B2,X, D]
    type d[X] = T[A,B,C,D] ~ T[A2,B2,C2,X]
    d.subst[d](c.subst[c](b.subst[b](a.subst[a](refl))))
  }

  /** We can lift equality into any type constructor */
  def lift5[T[_,_,_,_,_],A,A2,B,B2,C,C2,D,D2,E,E2](
    a: A ~ A2, b: B ~ B2, c: C ~ C2, d: D ~ D2,e : E ~ E2
  ) : (T[A,B,C,D,E] ~ T[A2,B2,C2,D2,E2]) = {
    type a[X] = T[A,B,C,D,E] ~ T[X, B, C, D, E]
    type b[X] = T[A,B,C,D,E] ~ T[A2,X, C, D, E]
    type c[X] = T[A,B,C,D,E] ~ T[A2,B2,X, D, E]
    type d[X] = T[A,B,C,D,E] ~ T[A2,B2,C2,X, E]
    type e[X] = T[A,B,C,D,E] ~ T[A2,B2,C2,D2,X]
    e.subst[e](d.subst[d](c.subst[c](b.subst[b](a.subst[a](refl)))))
  }

  /** This is an assertion that a type is injective. 
    * Pure Leibnizian equality cannot check injectivity without 'subtractive contexts', which Scala lacks. So we cheat.
    * This issue was raised in "Leibniz equality can be injective" (Oleg Kiselyov, Haskell Cafe Mailing List 2010):
    * http://osdir.com/ml/haskell-cafe@haskell.org/2010-05/msg00114.html
    */

  case class Injective[T[_]]()
  case class Injective2[T[_,_]]()
  case class Injective3[T[_,_,_]]()
  case class Injective4[T[_,_,_,_]]()
  case class Injective5[T[_,_,_,_,_]]()

  implicit def ListInjective = Injective[List]
  implicit def SetInjective = Injective[Set]
  implicit def EitherInjective = Injective2[Either]
  implicit def ValidationInjective = Injective2[Validation]

  implicit def Tuple2Injective = Injective2[Tuple2]
  implicit def Tuple3Injective = Injective3[Tuple3]
  implicit def Tuple4Injective = Injective4[Tuple4]
  implicit def Tuple5Injective = Injective5[Tuple5]

  implicit def Function0Injective = Injective[Function0]
  implicit def Function1Injective = Injective2[Function1]
  implicit def Function2Injective = Injective3[Function2]
  implicit def Function3Injective = Injective4[Function3]
  implicit def Function4Injective = Injective5[Function4]

  /** Unsafe coercion between types */
  def force[A,B] : (A ~ B) = new (A ~ B) { 
    def subst[F[_]](fa: F[A]) : F[B] = fa.asInstanceOf[F[B]]
  }

  def lower[T[_]:Injective,A,A2](
    t: T[A] ~ T[A2]
  ) : (A ~ A2)
    = force[A,A2]

  def lower2[T[_,_]:Injective2,A,A2,B,B2](
    t: T[A,B] ~ T[A2,B2]
  ) : (A ~ A2, B ~ B2)
    = (force[A,A2], force[B,B2])

  def lower3[T[_,_,_]:Injective3,A,A2,B,B2,C,C2](
    t: T[A,B,C] ~ T[A2,B2,C2]
  ) : (A ~ A2, B ~ B2, C ~ C2)
    = (force[A,A2], force[B,B2], force[C,C2])

  def lower4[T[_,_,_,_]:Injective4,A,A2,B,B2,C,C2,D,D2](
    t: T[A,B,C,D] ~ T[A2,B2,C2,D2]
  ) : (A ~ A2, B ~ B2, C ~ C2, D ~ D2)
    = (force[A,A2], force[B,B2], force[C,C2], force[D,D2])

  def lower5[T[_,_,_,_,_]:Injective5,A,A2,B,B2,C,C2,D,D2,E,E2](
    t: T[A,B,C,D,E] ~ T[A2,B2,C2,D2,E2]
  ) : (A ~ A2, B ~ B2, C ~ C2, D ~ D2, E ~ E2)
    = (force[A,A2], force[B,B2], force[C,C2], force[D,D2], force[E,E2])

  /*
  implicit leibnizFunction1Functor : GeneralizedFunctor[Id,~,Function1] = new GeneralizedFunctor[Id,~,Function1] {
    def dom = implicitly[Category[~]]
    def cod = implicitly[Category[Function1]]
    def fmap[A,B](f: A ~ B): A => B = witness(f)
  }
  */
}
