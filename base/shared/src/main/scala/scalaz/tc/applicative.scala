package scalaz
package tc

import java.lang.Throwable
import scala.language.experimental.macros
import scala.{Nothing, Unit}

import zio.{ Fiber, IO }

trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def pure[A](a: A): F[A]
}

object ApplicativeClass {

  trait DeriveMap[F[_]] extends ApplicativeClass[F] with MonadClass.Alt[DeriveMap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }

  implicit def fiberApplicative[E]: Applicative[Fiber[E, ?]] =
    instanceOf(new ApplicativeClass[Fiber[E, ?]] {
      def pure[A](a: A): Fiber[E, A] = new Fiber[E, A] {
        def join: IO[E, A]                             = IO.point(a)
        def interrupt(t: Throwable): IO[Nothing, Unit] = IO.unit
      }

      def ap[A, B](fa: Fiber[E, A])(f: Fiber[E, A => B]): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B] = fa.join.flatMap(a => f.join.map(f => f(a)))
        def interrupt(t: Throwable): IO[Nothing, Unit] = fa.interrupt(t) *> f.interrupt(t)
      }

      def map[A, B](ma: Fiber[E, A])(f: A => B): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B]                             = ma.join.map(f)
        def interrupt(t: Throwable): IO[Nothing, Unit] = ma.interrupt(t)
      }
    })
}

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro meta.Ops.i_0
  }
}
