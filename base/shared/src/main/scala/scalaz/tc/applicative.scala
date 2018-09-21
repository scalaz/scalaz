package scalaz
package tc

import scala.language.experimental.macros

import zio.Fiber

trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def pure[A](a: A): F[A]
}

object ApplicativeClass {

  trait DeriveMap[F[_]] extends ApplicativeClass[F] with MonadClass.Alt[DeriveMap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }

  implicit def fiberApplicative[E]: Applicative[Fiber[E, ?]] =
    instanceOf(new ApplicativeClass[Fiber[E, ?]] {
      def pure[A](a: A): Fiber[E, A] = Fiber.point(a)
      def ap[A, B](fa: Fiber[E, A])(f: Fiber[E, A => B]): Fiber[E, B] =
        (f zipWith fa)(_(_))
      def map[A, B](fa: Fiber[E, A])(f: A => B): Fiber[E, B] =
        fa.map(f)
    })
}

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro meta.Ops.i_0
  }
}
