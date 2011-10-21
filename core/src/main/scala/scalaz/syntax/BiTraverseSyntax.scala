package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BiTraverse` */
trait BiTraverseV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////
  def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit bt: BiTraverse[F], ap: Applicative[G]): G[F[C, D]] =
      bt.bitraverse(self)(f, g)
  ////
}

trait ToBiTraverseSyntax extends ToBiFunctorSyntax {
  implicit def ToBiTraverseV[F[_, _],A, B](v: F[A, B]) =
    new BiTraverseV[F,A, B] { def self = v }

  ////

  ////
}

trait BiTraverseSyntax[F[_, _]] extends BiFunctorSyntax[F] {
  implicit def ToBiTraverseV[A, B](v: F[A, B]): BiTraverseV[F, A, B] = new BiTraverseV[F, A, B] { def self = v }

  ////

  ////
}
