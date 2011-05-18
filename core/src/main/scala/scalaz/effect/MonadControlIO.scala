package scalaz
package effect

import IO._

trait MonadControlIO[F[_]] {
  val liftControlIO: LiftControlIO[F]
  val monad: Monad[F]

  def lftControlIO[A](f: RunInBase[F, IO] => IO[A]): F[A] =
    liftControlIO.liftControlIO(f)

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

object MonadControlIO extends MonadControlIOs

trait MonadControlIOs {
  def monadControlIO[F[_]](implicit l: LiftControlIO[F], m: Monad[F]): MonadControlIO[F] = new MonadControlIO[F] {
    val liftControlIO = l
    val monad = m
  }
}