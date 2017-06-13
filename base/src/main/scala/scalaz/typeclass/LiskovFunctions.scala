package scalaz
package typeclass

import data.===
import Liskov.<~<

trait LiskovFunctions {
  /**Lift Scala's subtyping relationship */
  def isa[A, B >: A]: A <~< B = Liskov.refl

  /** http://typelevel.org/blog/2014/03/09/liskov_lifting.html **/
  def liftCtf[F[_]: Contravariant, A, B](a: A <~< B): F[B] <~< F[A] = a.asInstanceOf[F[B] <~< F[A]]
  def liftCvf[A, B, F[_]: Functor](a: A <~< B): F[A] <~< F[B] = a.asInstanceOf[F[A] <~< F[B]]

  /**Subtyping is antisymmetric */
  //def antisymm[A, B, C](f: A <~< B, g: B <~< A): (A === B) = ???

  /**Subtyping is transitive */
  def trans[A, B, C](f: B <~< C, g: A <~< B): A <~< C =
    g.subst[λ[`-α` => α <~< C]](f)
}
