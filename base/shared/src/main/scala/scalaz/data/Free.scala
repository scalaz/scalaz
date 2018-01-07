package scalaz
package data

import scala.annotation.tailrec
import scalaz.data.Free.Impure

sealed abstract class Free[F[_], A] {

  final def map[B](f: A => B): Free[F, B] =
    flatMap(a => Free.pure(f(a)))

  final def flatMap[B](k: A => Free[F, B]): Free[F, B] =
    Free.impure(this, k)

  final def foldFree[B](la: F[Free[F, A]] => B)(ra: A => B)(implicit F: Functor[F]): B =
    resume.fold(la)(ra)

  @tailrec
  final def resume(implicit F: Functor[F]): F[Free[F, A]] \/ A =
    this match {
      case Free.Pure(a) => \/-(a)
      case Free.LiftF(fa) => -\/(F.map(fa)(Free.pure))
      case Free.Impure(ff, k) =>
        ff match {
          case Free.Pure(b) => k(b).resume
          case Free.LiftF(ga) => -\/(F.map(ga)(k))
          case Free.Impure(gg, j) => gg.flatMap(c => j(c).flatMap(k)).resume
        }
  }

  final def foldMap[M[_]](α: F ~> M)(implicit M: Monad[M]): M[A] =
    step match {
      case Free.Pure(a) => M.applicative.pure(a)
      case Free.LiftF(fa) => Forall.specialize[λ[α => F[α] => M[α]], A](α).apply(fa)
      case ff @ Free.Impure(_, _) => M.bind.flatMap(ff.ff foldMap α)(c => ff.kf(c).foldMap(α))
    }

  @tailrec
  private[Free] final def step: Free[F, A] =
    this match {
      case ff @ Impure(_, _) => ff.ff match {
        case gg @ Free.Impure(_, _) =>
          gg.ff.flatMap(a => gg.kf(a).flatMap(ff.kf)).step
        case Free.Pure(a)=> ff.kf(a).step
        case _ => ff
      }
      case f => f
    }
}

object Free {
  private[Free] final case class Pure[F[_], A] private (a: A) extends Free[F, A]
  private[Free] final case class LiftF[F[_], A] private (fa: F[A]) extends Free[F, A]
  private[Free] final case class Impure[F[_], EE, A] private (ff: Free[F, EE], kf: EE => Free[F, A]) extends Free[F, A] {
    type E = EE
    def fa: Free[F, E] = ff
    def k: E => Free[F, A] = kf
  }

  def pure[F[_], A](a: A): Pure[F, A] = Pure(a)
  def lift[F[_], A](fa: F[A]): LiftF[F, A] = LiftF(fa)
  def impure[F[_], E, A](ff: Free[F, E], k: E => Free[F, A]): Free[F, A] = Impure(ff, k)

  def wrap[F[_], A](ff: F[Free[F, A]]): Free[F, A] = lift(ff) flatMap identity
  def defer[F[_], A](f: Free[F, A]): Free[F, A] = pure(()) flatMap (_ => f)

}