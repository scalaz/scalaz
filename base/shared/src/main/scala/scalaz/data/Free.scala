package scalaz
package data

import scalaz.data.Free.Impure

sealed abstract class Free[F[_], A] {

  def map[B](f: A => B): Free[F, B] =
    bind(a => Free.pure(f(a)))

  def bind[B](k: A => Free[F, B]): Free[F, B] =
    Impure(this, k)

  //type ~>[F[_], G[_]] = ∀[λ[α => F[α] => G[α]]]
  final def foldFree[M[_]](α: F ~> M)(implicit M: Monad[M]): M[A] =
    this match {
      case Free.Pure(a) => M.applicative.pure(a)
      case Free.LiftF(fa) => Forall.specialize[λ[γ => F[γ] => M[γ]], A](α)(fa)
      case Free.Impure(ff, k) => ???
    }

}

object Free {
  private[Free] final case class Pure[F[_], A] private (a: A) extends Free[F, A]
  private[Free] final case class LiftF[F[_], A] private (fa: F[A]) extends Free[F, A]
  private[Free] final case class Impure[F[_], E, A](ff: Free[F, E], k: E => Free[F, A]) extends Free[F, A]

  def pure[F[_], A](a: A): Pure[F, A] = Pure(a)
  def lift[F[_], A](fa: F[A]): LiftF[F, A] = LiftF(fa)

  def runFree[F[_], A](f: Free[F, A]): F[Free[F, ?]] = ???

  def wrapFree[F[_], A](ff: F[Free[F, A]]): Free[F, A] =
    lift(ff).bind(identity)

}