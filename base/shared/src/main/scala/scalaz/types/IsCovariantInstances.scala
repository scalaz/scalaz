package scalaz
package types

trait IsCovariantInstances {
  implicit def scalaCovariant[F[+ _]]: IsCovariant[F] =
    instanceOf(new IsCovariantClass.SubstCv[F] {
      def substCv[G[+ _], A, B](g: G[F[A]])(implicit ev: A <~< B): G[F[B]] = {
        type H[+T] = G[F[T]]
        ev.substCv[H](g)
      }
    })
}
