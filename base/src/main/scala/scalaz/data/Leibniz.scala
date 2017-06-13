package scalaz
package data

import Identity.Id

trait ===[A, B] {
  def apply(a: A): B = subst[Id](a)
  def subst[F[_]](fa: F[A]): F[B]
  def onF[X](fa: X => A): X => B = subst[X => ?](fa)
}

object Leibniz {
  def refl[A]: A === A = new (A === A) {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }
}
