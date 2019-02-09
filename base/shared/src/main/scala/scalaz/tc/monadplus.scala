package scalaz
package tc

import Predef.Boolean

trait MonadPlusClass[F[_]] extends MonadClass[F] with ApplicativePlusClass[F] {

  /** Remove `f`-failing `A`s in `fa`, by which we mean: in the
   * expression `filter(filter(fa)(f))(g)`, `g` will never be invoked
   * for any `a` where `f(a)` returns false.
   */
  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    flatMap(fa)(a => if (f(a)) pure(a) else empty[A])

  /** Generalized version of Haskell's `catMaybes` */
  def unite[T[_], A](value: F[T[A]])(implicit T: Foldable[T]): F[A] =
    flatMap(value)((ta) => T.foldMap(ta)(a => pure(a))(monoid[A]))
}
