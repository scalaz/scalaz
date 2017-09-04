package scalaz
package typeclass

import Liskov.<~<

trait IsContravariantInstances {
  implicit def scalaContravariant[F[-_]]: IsContravariant[F] =
    new IsContravariantClass[F] with IsContravariantClass.SubstCv[F] {
      def substCv[G[+_], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
        type H[-T] = G[F[T]]
        ev.subst[H](g)
      }
    }
}
