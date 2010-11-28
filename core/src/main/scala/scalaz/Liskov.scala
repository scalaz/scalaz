package scalaz

/** Liskov substitutability : A better =<= 
  */

trait Liskov[-A,+B] {
  def subst[F[-_]](p: F[B]) : F[A]
}

object Liskov { 

  /** A convenient type alias for Leibniz */
  type <~:[-A,+B] = Liskov[A,B] 

  /** A flipped alias, for those used to their arrows running left to right */
  type :~>[+B,-A] = Liskov[A,B]

  /** Lift scala's subtyping relationship */
  implicit def subtype[A,B >: A] : A <~: B = new (A <~: B) {
    def subst[F[-_]](p: F[B]) : F[A] = p
  }

  /** We can witness equality by using it to convert between types */
  // implicit def witness[A,B](lt: A <~: B)(a: A) : B 

  /** Subtyping is reflexive */
  implicit def refl[A]: (A <~: A) = new (A <~: A) {
    def subst[F[_]](p:F[A]): F[A]= p
  }

  /** Subtyping is transitive */
  def trans[A,B,C](f: B <~: C, g: A <~: B) : A <~: C = {
    type g[-X] = X <~: C
    g.subst[g](f)
  }
 
  /** Subtyping forms a category */
  implicit def LiskovCategory: Category[<~:] = new Category[<~:] {
    def id[A] : (A <~: A) = refl[A]
    def compose[A,B,C](bc: B <~: C, ab: A <~: B) : (A <~: C) = trans(bc,ab)
  }

  /** We can lift subtyping into any covariant type constructor */
  def liftP[T[+_],A,A2](
    a: A <~: A2
  ) : (T[A] <~: T[A2]) = {
    type a[-X] = T[X] <~: T[A2]
    a.subst[a](refl)
  }

  /** We can lift subtyping into any contravariant type constructor */
  def liftN[T[-_],A,A2](
    a: A <~: A2
  ) : (T[A2] <~: T[A]) = {
    type a[-X] = T[A2] <~: T[X]
    a.subst[a](refl)
  }

  // def bracket[A,B](A <~: B, A :~> B): A ~ B
}
