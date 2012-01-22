package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BiTraverse` */
trait BiTraverseV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: BiTraverse[F]
  ////
  final def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit ap: Applicative[G]): G[F[C, D]] =
      F.bitraverse(self)(f, g)

  // Would be nice, but I'm not sure we can conjure UnapplyProduct implicitly, at least without multiple implicit
  // parameter lists.
  final def bitraverseU[GC, GD](f: A => GC, g: B => GD)(implicit G1: UnapplyProduct[Applicative, GC, GD]): G1.M[F[G1.A, G1.B]] =
      F.bitraverse(self)(a => G1._1(f(a)), b => G1._2(g(b)))(G1.TC)

  import Leibniz.===

  final def bisequence[G[_], A1, B1](implicit G: Applicative[G], eva: A === G[A1], evb: B === G[B1]): G[F[A1, B1]] =
    bitraverse(fa => eva(fa), fb => evb(fb))
  ////
}

trait ToBiTraverseV0 {
    implicit def ToBiTraverseVUnapply[FA](v: FA)(implicit F0: Unapply2[BiTraverse, FA]) =
      new BiTraverseV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: BiTraverse[F0.M] = F0.TC }
  
}

trait ToBiTraverseV extends ToBiTraverseV0 with ToBiFunctorV {
  
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
