package scalaz
package typeclass

/** A typeclass for types which are (covariant) [[Functor]]s in both type parameters.
 *
 * Minimal definition:
 * - `bimap`, or
 * - `lmap` and `rmap`
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
trait BifunctorClass[F[_, _]] extends TypeClass {

  def bimap[A, B, S, T](fab: F[A, B])(as: A => S, bt: B => T): F[S, T] =
    rmap(lmap(fab)(as))(bt)

  def lmap[A, B, S](fab: F[A, B])(as: A => S): F[S, B] =
    bimap(fab)(as, identity)

  def rmap[A, B, T](fab: F[A, B])(bt: B => T): F[A, T] =
    bimap(fab)(identity, bt)

}

