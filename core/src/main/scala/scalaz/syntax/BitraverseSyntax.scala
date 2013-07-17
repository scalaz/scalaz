package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bitraverse` */
sealed abstract class BitraverseOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Bitraverse[F]
  ////
  final def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit ap: Applicative[G]): G[F[C, D]] =
      F.bitraverseImpl(self)(f, g)

  // Would be nice, but I'm not sure we can conjure UnapplyProduct implicitly, at least without multiple implicit
  // parameter lists.
  final def bitraverseU[GC, GD](f: A => GC, g: B => GD)(implicit G1: UnapplyProduct[Applicative, GC, GD]): G1.M[F[G1.A, G1.B]] =
      F.bitraverseImpl(self)(a => G1._1(f(a)), b => G1._2(g(b)))(G1.TC)

  import Leibniz.===

  final def bisequence[G[_], A1, B1](implicit G: Applicative[G], eva: A === G[A1], evb: B === G[B1]): G[F[A1, B1]] =
    bitraverse(fa => eva(fa), fb => evb(fb))
  ////
}

trait ToBitraverseOps0 {
    implicit def ToBitraverseOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bitraverse, FA]) =
      new BitraverseOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Bitraverse[F0.M] = F0.TC }
  
}

trait ToBitraverseOps extends ToBitraverseOps0 with ToBifunctorOps with ToBifoldableOps {
  
  implicit def ToBitraverseOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bitraverse[F]) =
      new BitraverseOps[F,A, B] { def self = v; implicit def F: Bitraverse[F] = F0 }
  

  ////

  ////
}

trait BitraverseSyntax[F[_, _]] extends BifunctorSyntax[F] with BifoldableSyntax[F] {
  implicit def ToBitraverseOps[A, B](v: F[A, B]): BitraverseOps[F, A, B] = new BitraverseOps[F, A, B] { def self = v; implicit def F: Bitraverse[F] = BitraverseSyntax.this.F }

  def F: Bitraverse[F]
  ////

  ////
}
