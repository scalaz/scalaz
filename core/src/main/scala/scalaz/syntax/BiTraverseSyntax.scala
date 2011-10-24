package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BiTraverse` */
trait BiTraverseV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: BiTraverse[F]
  ////
  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit ap: Applicative[G]): G[F[C, D]] =
      F.bitraverse(self)(f, g)
  ////
}

trait ToBiTraverseSyntax extends ToBiFunctorSyntax {
  implicit def ToBiTraverseV[F[_, _],A, B](v: F[A, B])(implicit F0: BiTraverse[F]) =
    new BiTraverseV[F,A, B] { def self = v; implicit def F: BiTraverse[F] = F0 }

  ////

  ////
}

trait BiTraverseSyntax[F[_, _]] extends BiFunctorSyntax[F] {
  implicit def ToBiTraverseV[A, B](v: F[A, B])(implicit F0: BiTraverse[F]): BiTraverseV[F, A, B] = new BiTraverseV[F, A, B] { def self = v; implicit def F: BiTraverse[F] = F0 }

  ////

  ////
}
