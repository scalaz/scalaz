package scalaz
package typeclass

import Prelude._

sealed abstract class Liskov[-A, +B] {
  def subst[F[-_]](p: F[B]): F[A]

  def substCv[F[+_]](f: F[A]): F[B] = {
    type f[-a] = F[a] => F[B]
    subst[f](identity)(f)
  }

  def apply(a: A): B = {
    type f[+a] = a
    substCv[f](a)
  }
}

object Liskov extends LiskovTypes with LiskovInstances with LiskovFunctions {
  private def refl_[A]: (A <~< A) = new (A <~< A) {
    def subst[F[-_]](p: F[A]): F[A] = p
  }

  private val Refl = ∀.of[λ[α => α <~< α]].from(refl_)

  /**Subtyping is reflexive */
  implicit def refl[A]: (A <~< A) = Refl[A]
}
