package scalaz
package tc

class CompositionFunctorClass[F[_], G[_]](implicit F: FunctorClass[F], G: FunctorClass[G])
    extends FunctorClass[λ[α => F[G[α]]]] {

  def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] =
    F.map(fga)(G.map(_)(f))
}
