package scalaz
package tc

import java.lang.Throwable
import scala.language.experimental.macros
import scala.{ List, Nothing, Unit }

import zio.{ Fiber, IO }

@meta.minimal("pure", "unit")
trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def unit: F[Unit] = pure(())

  def pure[A](a: A): F[A] = map(unit)(_ => a)

  def map[A, B](ma: F[A])(f: A => B): F[B] = ap(ma)(pure(f))
}

object ApplicativeClass {

  implicit def fiberApplicative[E]: Applicative[Fiber[E, ?]] =
    instanceOf(new ApplicativeClass[Fiber[E, ?]] {
      override def pure[A](a: A): Fiber[E, A] = new Fiber[E, A] {
        def join: IO[E, A]                                     = IO.point(a)
        def interrupt0(ts: List[Throwable]): IO[Nothing, Unit] = IO.unit
      }

      override def ap[A, B](fa: Fiber[E, A])(f: Fiber[E, A => B]): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B]                                     = fa.join.flatMap(a => f.join.map(f => f(a)))
        def interrupt0(ts: List[Throwable]): IO[Nothing, Unit] = fa.interrupt0(ts) *> f.interrupt0(ts)
      }

      override def map[A, B](ma: Fiber[E, A])(f: A => B): Fiber[E, B] = new Fiber[E, B] {
        def join: IO[E, B]                                     = ma.join.map(f)
        def interrupt0(ts: List[Throwable]): IO[Nothing, Unit] = ma.interrupt0(ts)
      }
    })
}

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro ops.Ops.i_0
  }
}
