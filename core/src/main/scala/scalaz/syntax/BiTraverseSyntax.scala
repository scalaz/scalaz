package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BiTraverse` */
trait BiTraverseV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: BiTraverse[F]
  ////
  final def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit ap: Applicative[G]): G[F[C, D]] =
      F.bitraverse(self)(f, g)

  import Leibniz.===

  final def bisequence[G[_], A1, B1](implicit G: Applicative[G], eva: A === G[A1], evb: B === G[B1]): G[F[A1, B1]] =
    bitraverse(fa => eva(fa), fb => evb(fb))
  ////
}

trait ToBiTraverseV extends ToBiFunctorV {
    implicit def ToBiTraverseV[FA](v: FA)(implicit F0: Unapply2[BiTraverse, FA]) =
      new BiTraverseV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: BiTraverse[F0.M] = F0.TC }
  

  ////

  ////
}

trait BiTraverseSyntax[F[_, _]] extends BiFunctorSyntax[F] {
  implicit def ToBiTraverseV[A, B](v: F[A, B])(implicit F0: BiTraverse[F]): BiTraverseV[F, A, B] = new BiTraverseV[F, A, B] { def self = v; implicit def F: BiTraverse[F] = F0 }

  ////

  ////
}
