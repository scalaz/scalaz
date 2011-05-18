package scalaz
package effect

import data.Kleisli

trait MonadIO[F[_]] {
  val liftIO: LiftIO[F]
  val monad: Monad[F]

  def lftIO[A]: IO[A] => F[A] =
    liftIO.liftIO[A]

  def bind: Bind[F] = new Bind[F] {
    def bind[A, B](f: A => F[B]): F[A] => F[B] =
      monad.bd(f)
  }

  def pointed: Pointed[F] = new Pointed[F] {
    def point[A](a: => A): F[A] =
      monad.point(a)
  }

  def functor: Functor[F] = new Functor[F] {
    def fmap[A, B](f: A => B): F[A] => F[B] =
      monad.fmap(f)
  }

  def pointedFunctor: PointedFunctor[F] =
    monad.pointedFunctor

  def applic: Applic[F] =
    monad.applic

  def applicative: Applicative[F] =
    monad.applicative

  def fmap[A, B](f: A => B): F[A] => F[B] =
    monad.fmap(f)

  def apply[A, B](f: F[A => B]): F[A] => F[B] =
    monad.apply(f)

  def bd[A, B](f: A => F[B]): F[A] => F[B] =
    monad.bd(f)

  def jn[A]: F[F[A]] => F[A] =
    monad.jn[A]

  def point[A](a: => A): F[A] =
    monad.point(a)
}

object MonadIO extends MonadIOs

trait MonadIOs {
  def monadIO[F[_]](implicit l: LiftIO[F], m: Monad[F]): MonadIO[F] = new MonadIO[F] {
    val liftIO = l
    val monad = m
  }

  implicit val IOMonadIO: MonadIO[IO] =
    monadIO[IO]

  implicit def ReaderMonadIO[M[_], R](implicit mio: MonadIO[M]):
  MonadIO[({type λ[α] = Kleisli[R, M, α]})#λ] = {
    implicit val l = mio.liftIO
    implicit val m = mio.monad
    monadIO[({type λ[α] = Kleisli[R, M, α]})#λ]
  }

  implicit def RegionTMonadIO[M[_], S](implicit mio: MonadIO[M]):
  MonadIO[({type λ[α] = RegionT[S, M, α]})#λ] = {
    implicit val l = mio.liftIO
    implicit val m = mio.monad
    monadIO[({type λ[α] = RegionT[S, M, α]})#λ]
  }
}