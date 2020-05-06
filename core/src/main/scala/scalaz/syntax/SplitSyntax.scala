package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Split` */
final class SplitOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Split[F]) extends Ops[F[A, B]] {
  ////
  final def -*-[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.split(self, k)
  ////
}

sealed trait ToSplitOpsU[TC[F[_, _]] <: Split[F]] {
  implicit def ToSplitOpsUnapply[FA](v: FA)(implicit F0: Unapply2[TC, FA]): SplitOps[F0.M, F0.A, F0.B] =
    new SplitOps[F0.M, F0.A, F0.B](F0(v))(F0.TC)

}

trait ToSplitOps0[TC[F[_, _]] <: Split[F]] extends ToSplitOpsU[TC] {

  implicit def ToSplitOps[F[_, _],A, B](v: F[A, B])(implicit F0: TC[F]): SplitOps[F, A, B] =
    new SplitOps[F, A, B](v)


  implicit def ToSplitVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: TC[F[G, ?, ?]]): SplitOps[F[G, ?, ?], A, B] =
    new SplitOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ToSplitOps[TC[F[_, _]] <: Split[F]] extends ToSplitOps0[TC] with ToComposeOps[TC]

trait SplitSyntax[F[_, _]] extends ComposeSyntax[F] {
  implicit def ToSplitOps[A, B](v: F[A, B]): SplitOps[F, A, B] = new SplitOps[F, A, B](v)(SplitSyntax.this.F)

  def F: Split[F]
  ////

  ////
}
