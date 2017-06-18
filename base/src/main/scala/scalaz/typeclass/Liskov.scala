package scalaz
package typeclass

sealed abstract class Liskov[-A, +B] {
  def apply(a: A): B = Liskov.witness(this)(a)
  def subst[F[-_]](p: F[B]): F[A]
}

object Liskov extends LiskovTypes with LiskovInstances with LiskovFunctions {
  /**Subtyping is reflexive */
  implicit def refl[A]: (A <~< A) = new (A <~< A) {
    def subst[F[-_]](p: F[A]): F[A] = p
  }
}
