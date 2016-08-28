package scalaz
package effect

import scalaz.{IsomorphismFunctor, IsomorphismMonad}
import scalaz.Isomorphism.{<~>, <=>}

//
// Derive a type class instance through an Isomorphism for the effect type classes
//

trait IsomorphismResource[F, G] extends Resource[F] {
  implicit def G: Resource[G]

  def iso: F <=> G

  override def close(f: F): IO[Unit] =
    G.close(iso.to(f))
}

trait IsomorphismLiftIO[F[_], G[_]] extends LiftIO[F] {
  implicit def G: LiftIO[G]

  def iso: F <~> G

  override def liftIO[A](ioa: IO[A]): F[A] = iso.from(G.liftIO(ioa))
}

trait IsomorphismMonadIO[F[_], G[_]] extends MonadIO[F] with IsomorphismLiftIO[F, G] with IsomorphismMonad[F, G] {
  implicit def G: MonadIO[G]
}

trait IsomorphismMonadCatchIO[F[_], G[_]] extends MonadCatchIO[F] with IsomorphismMonadIO[F, G] {
  implicit def G: MonadCatchIO[G]

  override def except[A](ma: F[A])(handler: (Throwable) => F[A]): F[A] =
    iso.from(G.except(iso.to(ma))(t => iso.to(handler(t))))
}

trait IsomorphismLiftControlIO[F[_], G[_]] extends LiftControlIO[F] {
  import IO._

  implicit def G: LiftControlIO[G]

  implicit def iso: F <~> G

  override def liftControlIO[A](f: RunInBase[F, IO] => IO[A]): F[A] =
    iso.from(G.liftControlIO(f compose hoistRunInBase[F, G]))
}

trait IsomorphismMonadControlIO[F[_], G[_]] extends MonadControlIO[F] with IsomorphismLiftControlIO[F, G] with IsomorphismMonad[F, G] {
  implicit def G: MonadControlIO[G]
}
