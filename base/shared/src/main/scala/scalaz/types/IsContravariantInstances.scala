package scalaz
package types

trait IsContravariantInstances {
  implicit def scalaContravariant[F[- _]]: IsContravariant[F] =
    instanceOf(new IsContravariantClass.SubstCv[F] {
      def substCv[G[+ _], A, B](g: G[F[B]])(implicit ev: A <~< B): G[F[A]] = {
        type H[-T] = G[F[T]]
        ev.substCt[H](g)
      }
    })
}
