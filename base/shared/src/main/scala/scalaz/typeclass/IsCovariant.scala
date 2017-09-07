package scalaz
package typeclass

import Liskov.<~<

/**
 * Witnesses that the type constructor `F[_]` is covariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
trait IsCovariant[F[_]] {
  def substCv[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]]

  def substCt[G[-_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]]

  def liftLiskov[A, B](implicit ev: A <~< B): F[A] <~< F[B]

  def widen[A, B](fa: F[A])(implicit ev: A <~< B): F[B]
}

object IsCovariant extends IsCovariantInstances {
  def apply[F[_]](implicit F: IsCovariant[F]): IsCovariant[F] = F
}
