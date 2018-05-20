package scalaz
package ct

import language.experimental.macros

trait BifunctorSyntax {
  implicit final class ToBifunctorOps[F[_, _]: Bifunctor, A, B](ma: F[A, B]) {
    def bimap[S, T](f: A => S, g: B => T): F[S, T] = macro meta.Ops.f_2

    def lmap[S](f: A => S): F[S, B] = macro meta.Ops.f_1
    def rmap[T](f: B => T): F[A, T] = macro meta.Ops.f_1
  }
}
