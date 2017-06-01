package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bitraverse` */
final class BitraverseOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Bitraverse[F]) extends Ops[F[A, B]] {
  ////
  final def bitraverse[G[_], C, D](f: A => G[C], g: B => G[D])(implicit ap: Applicative[G]): G[F[C, D]] =
      F.bitraverseImpl(self)(f, g)

  final def bitraverseU[GC, GD](f: A => GC, g: B => GD)(implicit G1: UnapplyProduct[Applicative, GC, GD]): G1.M[F[G1.A, G1.B]] =
      F.bitraverseImpl(self)(a => G1._1(f(a)), b => G1._2(g(b)))(G1.TC)

  import Leibniz.===

  final def bisequence[G[_], A1, B1](implicit G: Applicative[G], eva: A === G[A1], evb: B === G[B1]): G[F[A1, B1]] =
    bitraverse(fa => eva(fa), fb => evb(fb))

  final def bisequenceU[GC, GD](implicit eva: A === GC, evb: B === GD, G1: UnapplyProduct[Applicative, GC, GD]): G1.M[F[G1.A, G1.B]] =
    bitraverseU(eva, evb)
  ////
}

sealed trait ToBitraverseOps0 {
  implicit def ToBitraverseOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bitraverse, FA]) =
    new BitraverseOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToBitraverseOps extends ToBitraverseOps0 with ToBifunctorOps with ToBifoldableOps {

  implicit def ToBitraverseOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bitraverse[F]) =
    new BitraverseOps[F,A, B](v)


  implicit def ToBitraverseVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Bitraverse[F[G, ?, ?]]) =
    new BitraverseOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait BitraverseSyntax[F[_, _]] extends BifunctorSyntax[F] with BifoldableSyntax[F] {
  implicit def ToBitraverseOps[A, B](v: F[A, B]): BitraverseOps[F, A, B] = new BitraverseOps[F, A, B](v)(BitraverseSyntax.this.F)

  def F: Bitraverse[F]
  ////

  ////
}
