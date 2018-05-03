package scalaz
package typeclass

import scala.language.experimental.macros

trait ApplySyntax {
  implicit final class ToApplyOps[F[_]: Apply, A](fa: F[A]) {
    def ap[B](f: F[A => B]): F[B] = macro meta.Ops.f_1

    def liftA2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fb.ap(Apply[F].map(fa)(a => f(a, _)))
  }
}
