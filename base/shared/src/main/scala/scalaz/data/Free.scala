package scalaz
package data

import scala.annotation.tailrec
import scalaz.data.Free.Impure

/**
 * When choosing a Free monad, remember to consider your use-case - asymoptotics and constant factors
 * vary from implementation to implementation.
 *
 * [View Ed Kmett's comments here](https://www.reddit.com/r/haskell/comments/7q4sku/are_people_using_freer_monads_or_still_mostly/dsmlnh7/)
 *
 */
sealed abstract class Free[F[_], A] {

  final def map[B](f: A => B): Free[F, B] =
    flatMap(a => Free.pure(f(a)))

  final def flatMap[B](k: A => Free[F, B]): Free[F, B] =
    Free.impure(this, k)

  final def hoist[G[_]](α: F ~> Free[G, ?]): Free[G, A] =
    foldMap(α)(Free.monad)

  final def foldFree[B](la: F[Free[F, A]] => B)(ra: A => B)(implicit F: Functor[F]): B =
    runFree.fold(la)(ra)

  final def foldMap[M[_]](α: F ~> M)(implicit M: Monad[M]): M[A] =
    step match {
      case Free.Pure(a)       => M.pure(a)
      case Free.LiftF(fa)     => Forall.specialize[λ[α => F[α] => M[α]], A](α).apply(fa)
      case Free.Impure(ff, k) => M.flatMap(ff foldMap α)(c => k(c).foldMap(α))
    }

  @tailrec
  final def runFree(implicit F: Functor[F]): F[Free[F, A]] \/ A =
    this match {
      case Free.Pure(a)   => \/-(a)
      case Free.LiftF(fa) => -\/(F.map(fa)(Free.pure))
      case Free.Impure(ff, k) =>
        ff match {
          case Free.Pure(b)       => k(b).runFree
          case Free.LiftF(ga)     => -\/(F.map(ga)(k))
          case Free.Impure(gg, j) => gg.flatMap(c => j(c).flatMap(k)).runFree
        }
    }

  @tailrec
  private[Free] final def step: Free[F, A] =
    this match {
      case ff @ Impure(_, _) =>
        ff.ff match {
          case gg @ Free.Impure(_, _) => gg.ff.flatMap(a => gg.k(a).flatMap(ff.k)).step
          case Free.Pure(a)           => ff.k(a).step
          case _                      => ff
        }
      case f => f
    }
}

object Free extends FreeInstances {
  final case class Pure[F[_], A](a: A)      extends Free[F, A]
  final case class LiftF[F[_], A](fa: F[A]) extends Free[F, A]
  final case class Impure[F[_], EE, A](ff: Free[F, EE], k: EE => Free[F, A]) extends Free[F, A] {
    type E = EE
    def fa: Free[F, E]      = ff
    def kk: E => Free[F, A] = k
  }

  def pure[F[_], A](a: A): Pure[F, A]                                    = Pure(a)
  def lift[F[_], A](fa: F[A]): LiftF[F, A]                               = LiftF(fa)
  def impure[F[_], E, A](ff: Free[F, E], k: E => Free[F, A]): Free[F, A] = Impure(ff, k)

  def wrap[F[_], A](ff: F[Free[F, A]]): Free[F, A] = lift(ff) flatMap identity
  def defer[F[_], A](f: Free[F, A]): Free[F, A]    = pure(()) flatMap (_ => f)
}
