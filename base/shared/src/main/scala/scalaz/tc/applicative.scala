package scalaz
package tc

import scala.List
import scala.Unit
import scala.language.experimental.macros

import zio.Fiber

@meta.minimal("pure", "unit")
trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def unit: F[Unit] = pure(())

  def pure[A](a: A): F[A] = map(unit)(_ => a)

  def map[A, B](ma: F[A])(f: A => B): F[B] = ap(ma)(pure(f))
}

object ApplicativeClass {

  implicit val instanceList: Applicative[List] = instanceOf(instances.list.control)

  implicit def fiberApplicative[E]: Applicative[Fiber[E, ?]] =
    instanceOf(new ApplicativeClass[Fiber[E, ?]] {
      override def pure[A](a: A): Fiber[E, A] = Fiber.point(a)
      override def ap[A, B](fa: Fiber[E, A])(f: Fiber[E, A => B]): Fiber[E, B] =
        (f zipWith fa)(_(_))
      override def map[A, B](fa: Fiber[E, A])(f: A => B): Fiber[E, B] =
        fa.map(f)
    })
}

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro ops.Ops.i_0
  }
}
