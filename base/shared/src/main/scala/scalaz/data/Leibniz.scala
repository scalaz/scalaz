package scalaz
package data

import Prelude._
import Identity.Id

sealed trait ===[A, B] {
  def apply(a: A): B = subst[Id](a)
  def subst[F[_]](fa: F[A]): F[B]
  def onF[X](fa: X => A): X => B = subst[X => ?](fa)
}

object Leibniz {
  private def refl_[A]: A === A = new (A === A) {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }

  private val Refl = ∀.of[λ[α => α === α]].from(refl_)

  def refl[A]: A === A = Refl[A]
}
