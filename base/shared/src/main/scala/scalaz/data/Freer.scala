package scalaz
package data

import scala.annotation.tailrec


sealed abstract class Freer[F[_], A] {

  def map[B](f: A => B): Freer[F, B] =
    flatMap[B](a => Freer.pure[F, B](f(a)))

  def flatMap[B](k: A => Freer[F, B]): Freer[F, B] =
    this match {
      case Freer.Pure(a) => k(a)
      case l @ Freer.LiftF(_) => Freer.impure(l, Freer.singleK(Kleisli.wrapKleisli(k)))
      case Freer.Impure(ff, rs) => Freer.impure(ff, rs :+ Kleisli.wrapKleisli(k))
    }

  final def hoist[G[_]](α: F ~> Freer[G, ?]): Freer[G, A] =
    foldMap(α)(Freer.monad)

  final def foldFreer[B](la: F[Freer[F, A]] => B)(ra: A => B)(implicit F: Functor[F]): B =
    runFreer.fold(la)(ra)

  @tailrec
  final def foldMap[M[_]](α: F ~> M)(implicit M: Monad[M]): M[A] =
    this match {
      case Freer.Pure(a) => M.applicative.pure(a)
      case Freer.LiftF(fa) => Forall.specialize[λ[α => F[α] => M[α]], A](α).apply(fa)
      case Freer.Impure(f, q) =>
        M.bind.flatMap(f foldMap α)(a => Kleisli.runKleisli(Freer.runQuiver(q)).apply(a).foldMap(α))
    }

  @tailrec
  def runFreer(implicit F: Functor[F]): F[Freer[F, A]] \/ A =
    this match {
      case Freer.Pure(a) => \/-(a)
      case Freer.LiftF(fa) => -\/(F.map(fa)(Freer.pure))
      case Freer.Impure(f, q) =>
        f match {
          case Freer.Pure(e) => Kleisli.runKleisli(Freer.runQuiver(q)).apply(e).runFreer
          case Freer.LiftF(fe) => -\/(F.map(fe)(Kleisli.runKleisli(Freer.runQuiver(q))))
          case Freer.Impure(ff, qq) =>
            Freer.impure(ff, qq :+ Freer.runQuiver(q)).runFreer
        }
    }
}

object Freer extends FreerInstances {
  private[Freer] type =>:[F[_], A, B] = Kleisli[Freer[F, ?], A, B]
  private[Freer] type Quiver[F[_], A, B] = ACatenable1[=>:[F, ?, ?], A, B]

  final case class Pure[F[_], A] private (a: A) extends Freer[F, A]
  final case class LiftF[F[_], A] private (fa: F[A]) extends Freer[F, A]
  final case class Impure[F[_], E, A] private (f: Freer[F, E], q: Quiver[F, E, A]) extends Freer[F, A] {
    type EE = E
    def ff: Freer[F, EE] = f
    def qq: Quiver[F, E, A] = q
  }

  def pure[F[_], A](a: A): Pure[F, A] = Pure(a)
  def impure[F[_], E, A](ff: Freer[F, E], rs: Quiver[F, E, A]): Freer[F, A] = Impure(ff, rs)
  def lift[F[_], A](fa: F[A]): Freer[F, A] = LiftF(fa)

  def defer[F[_], A](f: Freer[F, A]): Freer[F, A] = pure(()) flatMap (_ => f)
  def singleF[F[_], A, B](f: A => Freer[F, B]): Quiver[F, A, B] = singleK(Kleisli.wrapKleisli[Freer[F, ?], A, B](f))
  def singleK[F[_], A, B](r: =>:[F, A, B]): Quiver[F, A, B] = ACatenable1.lift[=>:[F, ?, ?], A, B](r)

  private[Freer] def runQuiver[F[_], A, B](q: Freer.Quiver[F, A, B]): =>:[F, A, B] =
    q.fold(KleisliImpl.kleisliCompose[Freer[F, ?]])

}
