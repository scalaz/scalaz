package scalaz
package core

import java.lang.Throwable

import algebra.MonoidClass
import ct._
import types.IsCovariantClass
import zio.{ Fiber, IO }

trait IOInstances {
  implicit def ioMonad[E]: Monad[IO[E, ?]] =
    instanceOf(new MonadClass[IO[E, ?]] with BindClass.DeriveFlatten[IO[E, ?]] {
      override final def map[A, B](ma: IO[E, A])(f: A => B): IO[E, B] =
        ma.map(f)

      override final def ap[A, B](ma: IO[E, A])(mf: IO[E, A => B]): IO[E, B] =
        ma.flatMap(a => mf.map(f => f(a)))

      override final def pure[A](a: A): IO[E, A] = IO.now(a)

      override final def flatMap[A, B](ma: IO[E, A])(f: A => IO[E, B]): IO[E, B] =
        ma.flatMap(f)
    })

  implicit final val ioBifunctor: Bifunctor[IO] =
    instanceOf(new BifunctorClass.DeriveBimap[IO] {
      override def lmap[A, B, S](fab: IO[A, B])(as: A => S): IO[S, B] = fab.leftMap(as)
      override def rmap[A, B, T](fab: IO[A, B])(bt: B => T): IO[A, T] = fab.map(bt)
    })

  implicit def ioIsCovariant[E]: IsCovariant[IO[E, ?]] = IsCovariantClass.unsafeForce[IO[E, ?]]

}

trait FiberInstances {
  implicit def fiberMonoid[E, A](implicit A: Monoid[A]): Monoid[Fiber[E, A]] =
    instanceOf(new MonoidClass[Fiber[E, A]] {
      def mappend(a1: Fiber[E, A], a2: => Fiber[E, A]) = a1.zipWith(a2)(A.mappend(_, _))
      def mempty = new Fiber[E, A] {
        def join: IO[E, A]                            = IO.now(A.mempty)
        def interrupt[E2](t: Throwable): IO[E2, Unit] = IO.unit[E2]
      }
    })

  implicit def fiberApplicative[E]: Applicative[Fiber[E, ?]] =
    instanceOf(new ApplicativeClass[Fiber[E, ?]] {
      final def pure[A](a: A): Fiber[E, A] = new Fiber[E, A] {
        def join: IO[E, A]                            = IO.point(a)
        def interrupt[E2](t: Throwable): IO[E2, Unit] = IO.unit[E2]
      }

      final def ap[A, B](fa: Fiber[E, A])(f: Fiber[E, A => B]): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B] = fa.join.flatMap(a => f.join.map(f => f(a)))

        def interrupt[E2](t: Throwable): IO[E2, Unit] = fa.interrupt(t) *> f.interrupt(t)
      }

      final def map[A, B](ma: Fiber[E, A])(f: A => B): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B]                            = ma.join.map(f)
        def interrupt[E2](t: Throwable): IO[E2, Unit] = ma.interrupt(t)
      }
    })
}
