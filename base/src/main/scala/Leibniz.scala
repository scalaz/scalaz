package scalaz

import Identity.Id

object Leibniz {
  trait ===[A, B] {
    def subst[F[_]](fa: F[A]): F[B]

    def apply(a: A): B = subst[Id](a)
    def onF[X](fa: X => A): X => B = subst[X => ?](fa)
  }

  def refl[A]: A === A = new (A === A) {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }
}
