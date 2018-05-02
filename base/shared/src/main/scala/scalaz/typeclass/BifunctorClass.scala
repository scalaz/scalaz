package scalaz
package typeclass

/** A typeclass for types which are (covariant) [[Functor]]s in both type parameters.
 *
 * Minimal definition:
 * - `bimap` (using [[BifunctorClass.DeriveLmapRmap]])
 * - `lmap` and `rmap` (using [[BifunctorClass.DeriveBimap]])
 *
 * The laws for a [[Bifunctor]] instance are:
 * - for all `A`, `Bifunctor[A, ?]` must be a lawful [[Functor]] (with `map == rmap`);
 * - for all `B`, `Bifunctor[?, B]` must be a lawful [[Functor]] (with `map == lmap`); and
 * - for all types `A, B, S, T` functions `as: A => S, bt: B => T`, and `fab: F[A, B]`:
 *   ```scala
 *       lmap(rmap(fab)(bt))(as) === rmap(lmap(fab)(as))(bt) === bimap(fab)(as, bt)
 *   ```
 */
trait BifunctorClass[F[_, _]] {

  def bimap[A, B, S, T](fab: F[A, B])(as: A => S, bt: B => T): F[S, T]

  def lmap[A, B, S](fab: F[A, B])(as: A => S): F[S, B]

  def rmap[A, B, T](fab: F[A, B])(bt: B => T): F[A, T]

}

object BifunctorClass {

  trait DeriveLmapRmap[F[_, _]] extends BifunctorClass[F] with Alt[DeriveLmapRmap[F]] {
    def lmap[A, B, S](fab: F[A, B])(as: A => S): F[S, B] = bimap(fab)(as, identity)
    def rmap[A, B, T](fab: F[A, B])(bt: B => T): F[A, T] = bimap(fab)(identity, bt)
  }

  trait DeriveBimap[F[_, _]] extends BifunctorClass[F] with Alt[DeriveBimap[F]] {
    def bimap[A, B, S, T](fab: F[A, B])(as: A => S, bt: B => T): F[S, T] = rmap(lmap(fab)(as))(bt)
  }

  sealed trait Alt[D <: Alt[D]]

  trait BifunctorFunctorTemplate[F[_, _], A] extends FunctorClass[F[A, ?]] {
    val F: Bifunctor[F]

    def map[B, BB](ma: F[A, B])(f: B => BB): F[A, BB] = F.rmap(ma)(f)
  }
}
