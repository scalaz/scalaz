package scalaz
package typeclass

import Liskov.<~<

trait LiskovFunctions {
  /**Lift Scala's subtyping relationship */
  def isa[A, B >: A]: A <~< B = Liskov.refl

  def liftCt[F[_], A, B](a: A <~< B)(implicit F: IsContravariant[F]): F[B] <~< F[A] =
    F.liftLiskov[A, B](a)

  def liftCv[F[_], A, B](a: A <~< B)(implicit F: IsCovariant[F]): F[A] <~< F[B] =
    F.liftLiskov[A, B](a)

  /**Subtyping is antisymmetric */
  //def antisymm[A, B, C](f: A <~< B, g: B <~< A): (A === B) = ???

  /**Subtyping is transitive */
  def trans[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
    g.subst[λ[`-α` => α <~< C]](f)

  def unsafeForce[A, B]: A <~< B =
    Liskov.refl[A].asInstanceOf[A <~< B]
}
