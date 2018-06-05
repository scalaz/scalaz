package scalaz
package ct

import scala.language.experimental.macros

trait ApplyClass[F[_]] extends FunctorClass[F] {
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

trait ApplyFunctions {
  @inline final def ap[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Apply[F]): F[B] =
    F.ap(fa)(f)
}

trait ApplySyntax {
  implicit final class ToApplyOps[F[_], A](fa: F[A]) {
    def ap[B](f: F[A => B])(implicit ev: Apply[F]): F[B] =
      macro meta.Ops.i_1

    def liftA2[B, C](fb: F[B])(f: (A, B) => C)(implicit ev: Apply[F]): F[C] =
      fb.ap(Apply[F].map(fa)(a => f(a, _)))
  }
}
