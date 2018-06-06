package scalaz
package ct

import scala.Tuple2

import scala.language.experimental.macros

/** A typeclass for types which are (covariant) [[Functor]]s in both type parameters.
 *
 * Minimal definition:
 * - `bimap`, _or_
 * - `lmap` _and_ `rmap`.
 *
 * The laws for a [[Bifunctor]] instance are:
 * - for all `A`, `Bifunctor[A, ?]` must be a lawful [[Functor]] (with `map == rmap`);
 * - for all `B`, `Bifunctor[?, B]` must be a lawful [[Functor]] (with `map == lmap`); and
 * - for all types `A, B, S, T` functions `as: A => S, bt: B => T`, and `fab: F[A, B]`:
 *   ```scala
 *       lmap(rmap(fab)(bt))(as) === rmap(lmap(fab)(as))(bt) === bimap(fab)(as, bt)
 *   ```
 */
@meta.minimal("bimap", ("lmap", "rmap"))
trait BifunctorClass[F[_, _]] {

  def bimap[A, B, S, T](fab: F[A, B])(as: A => S, bt: B => T): F[S, T] =
    rmap(lmap(fab)(as))(bt)

  def lmap[A, B, S](fab: F[A, B])(as: A => S): F[S, B] =
    bimap(fab)(as, identity)

  def rmap[A, B, T](fab: F[A, B])(bt: B => T): F[A, T] =
    bimap(fab)(identity, bt)

}

trait BifunctorInstances {

  implicit final val tuple2Bifunctor: Bifunctor[Tuple2] =
    instanceOf(new BifunctorClass[Tuple2] {
      override def lmap[A, B, S](fab: (A, B))(f: A => S): (S, B) = fab.copy(_1 = f(fab._1))
      override def rmap[A, B, T](fab: (A, B))(f: B => T): (A, T) = fab.copy(_2 = f(fab._2))
    })
}

trait BifunctorSyntax {
  implicit final class ToBifunctorOps[F[_, _]: Bifunctor, A, B](ma: F[A, B]) {
    def bimap[S, T](f: A => S, g: B => T): F[S, T] = macro meta.Ops.f_2

    def lmap[S](f: A => S): F[S, B] = macro meta.Ops.f_1
    def rmap[T](f: B => T): F[A, T] = macro meta.Ops.f_1
  }
}
