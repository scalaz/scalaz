package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bitraverse` */
trait BitraverseV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Bitraverse[F]
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

trait ToBitraverseV0 {
    implicit def ToBitraverseVUnapply[FA](v: FA)(implicit F0: Unapply2[Bitraverse, FA]) =
      new BitraverseV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Bitraverse[F0.M] = F0.TC }
  
}

trait ToBitraverseV extends ToBitraverseV0 with ToBifunctorV {
  
  implicit def ToBitraverseV[F[_, _],A, B](v: F[A, B])(implicit F0: Bitraverse[F]) =
      new BitraverseV[F,A, B] { def self = v; implicit def F: Bitraverse[F] = F0 }
  

  ////

  ////
}

trait BitraverseSyntax[F[_, _]] extends BifunctorSyntax[F] {
  implicit def ToBitraverseV[A, B](v: F[A, B])(implicit F0: Bitraverse[F]): BitraverseV[F, A, B] = new BitraverseV[F, A, B] { def self = v; implicit def F: Bitraverse[F] = F0 }

  ////

  ////
}
