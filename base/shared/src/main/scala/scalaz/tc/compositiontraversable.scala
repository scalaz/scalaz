package scalaz
package tc

class CompositionTraversableClass[F[_], G[_]](implicit F: TraversableClass[F], G: TraversableClass[G])
    extends CompositionFunctorClass[F, G]
    with TraversableClass[λ[α => F[G[α]]]] {

  def foldLeft[A, B](fga: F[G[A]], z: B)(f: (B, A) => B): B =
    F.foldLeft(fga, z) { (b, ga) =>
      G.foldLeft(ga, b)(f)
    }

  def foldRight[A, B](fga: F[G[A]], z: => B)(f: (A, => B) => B): B =
    F.foldRight(fga, z) { (ga, b) =>
      G.foldRight(ga, b)(f)
    }

  override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
    F.traverse(fga)(G.traverse(_)(f))
}
