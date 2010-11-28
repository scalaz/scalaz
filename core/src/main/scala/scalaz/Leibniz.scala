package scalaz

/** Leibnizian equality: A better =:= */
trait Leibniz[A,B] {
  def apply[F[_]](p: F[A]) : F[B]

  final def *[+[_,_],C,D](that: Leibniz[C,D]) : Leibniz[A+C,B+D] = Leibniz.lift2(this,that)
  final def andThen[C](that: Leibniz[B,C]) : Leibniz[A,C] = Leibniz.trans(that,this)
  final def compose[C](that: Leibniz[C,A]) : Leibniz[C,B] = Leibniz.trans(this,that)
  final def inverse : Leibniz[B,A] = Leibniz.symm(this)
}

object Leibniz { 
  /** A convenient type alias for Leibniz */
  type ~[A,B] = Leibniz[A,B] 

  /** Equality is reflexive */
  def refl[A] = new (A ~ A) {
    def apply[F[_]](p:F[A]): F[A]= p
  }

  /** Equality is transitive */
  def trans[A,B,C](f: B ~ C, g: A ~ B) = new (A ~ C) {
    def apply[F[_]](p:F[A]): F[C] = f[PartialApply1Of2[~,A]#Apply](g)(p)
  }
 
  /** Equality is symmetric */
  def symm[A,B](f: A ~ B) : B ~ A = new (B ~ A) {
    def apply[F[_]](p:F[B]): F[A] = f[PartialApply1Of2[~,A]#Flip](refl)(p)
  }

  /** Equivalence forms a category */
  implicit def LeibnizCategory: Category[~] = new Category[~] {
    def id[A] : (A ~ A) = refl[A]
    def compose[A,B,C](bc: B ~ C, ab: A ~ B) : (A ~ C) = trans(bc,ab)
  }

  /** We can lift equality into any type constructor */
  def lift[T[_],A,A2](
    a: A ~ A2
  ) : (T[A] ~ T[A2]) = 
  new (T[A] ~ T[A2]) {
    def apply[F[_]](p: F[T[A]]) : F[T[A2]] = {
      type a[X] = T[A] ~ T[X]
      a[a](refl)(p)
    }
  }

  /** We can lift equality into any type constructor */
  def lift2[T[_,_],A,A2,B,B2](
    a: A ~ A2, b: B ~ B2
  ) : (T[A,B] ~ T[A2,B2]) = 
  new (T[A,B] ~ T[A2,B2]) {
    def apply[F[_]](p : F[T[A,B]]) : F[T[A2,B2]] = {
      type a[X] = T[A,B] ~ T[X, B]
      type b[X] = T[A,B] ~ T[A2,X]
      b[b](a[a](refl))(p)
    }
  }

  /** We can lift equality into any type constructor */
  def lift3[T[_,_,_],A,A2,B,B2,C,C2](
    a: A ~ A2, b: B ~ B2, c: C ~ C2
  ) : (T[A,B,C] ~ T[A2,B2,C2]) = 
  new (T[A,B,C] ~ T[A2,B2,C2]) {
    def apply[F[_]](p: F[T[A,B,C]]) : F[T[A2,B2,C2]] = {
      type a[X] = T[A,B,C] ~ T[X ,B, C]
      type b[X] = T[A,B,C] ~ T[A2,X, C]
      type c[X] = T[A,B,C] ~ T[A2,B2,X]
      c[c](b[b](a[a](refl)))(p)
    }
  }

  /** We can lift equality into any type constructor */
  def lift4[T[_,_,_,_],A,A2,B,B2,C,C2,D,D2](
    a: A ~ A2, b: B ~ B2, c: C ~ C2, d: D ~ D2
  ) : (T[A,B,C,D] ~ T[A2,B2,C2,D2]) = 
  new (T[A,B,C,D] ~ T[A2,B2,C2,D2]) {
    def apply[F[_]](p: F[T[A,B,C,D]]) : F[T[A2,B2,C2,D2]] = {
      type a[X] = T[A,B,C,D] ~ T[X, B, C, D]
      type b[X] = T[A,B,C,D] ~ T[A2,X, C, D]
      type c[X] = T[A,B,C,D] ~ T[A2,B2,X, D]
      type d[X] = T[A,B,C,D] ~ T[A2,B2,C2,X]
      d[d](c[c](b[b](a[a](refl))))(p)
    }
  }

  /** We can lift equality into any type constructor */
  def lift5[T[_,_,_,_,_],A,A2,B,B2,C,C2,D,D2,E,E2](
    a: A ~ A2, b: B ~ B2, c: C ~ C2, d: D ~ D2,e : E ~ E2
  ) : (T[A,B,C,D,E] ~ T[A2,B2,C2,D2,E2]) = 
  new (T[A,B,C,D,E] ~ T[A2,B2,C2,D2,E2]) {
    def apply[F[_]](p: F[T[A,B,C,D,E]]) : F[T[A2,B2,C2,D2,E2]] = {
      type a[X] = T[A,B,C,D,E] ~ T[X, B, C, D, E]
      type b[X] = T[A,B,C,D,E] ~ T[A2,X, C, D, E]
      type c[X] = T[A,B,C,D,E] ~ T[A2,B2,X, D, E]
      type d[X] = T[A,B,C,D,E] ~ T[A2,B2,C2,X, E]
      type e[X] = T[A,B,C,D,E] ~ T[A2,B2,C2,D2,X]
      e[e](d[d](c[c](b[b](a[a](refl)))))(p)
    }
  }
}
