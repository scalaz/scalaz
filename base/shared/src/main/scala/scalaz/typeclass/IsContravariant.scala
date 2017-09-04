package scalaz
package typeclass

import Liskov.<~<

/**
 * Witnesses that the type constructor `F[_]` is contravariant,
 * even though the variance annotation of its type parameter has been forgotten.
 *
 * A safer alternative to
 * https://typelevel.org/blog/2014/03/09/liskov_lifting.html
 */
trait IsContravariant[F[_]] {
  def substCv[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]]

  def substCt[G[-_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]]

  def liftLiskov[A, B](implicit ev: A <~< B): F[B] <~< F[A]

  def widen[A, B](fa: F[B])(implicit ev: A <~< B): F[A]
}

object IsContravariant extends IsContravariantInstances {
  def apply[F[_]](implicit F: IsContravariant[F]): IsContravariant[F] = F
}
