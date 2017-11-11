package scalaz
package typeclass

import Liskov.<~<

trait IsCovariantInstances {
  implicit def scalaCovariant[F[+_]]: IsCovariant[F] =
    new IsCovariantClass[F] with IsCovariantClass.SubstCv[F] {
      def substCv[G[+_], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
        type H[+T] = G[F[T]]
        ev.substCv[H](g)
      }
    }
}
