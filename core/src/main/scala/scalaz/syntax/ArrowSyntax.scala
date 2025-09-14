package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arrow` */
final class ArrowOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Arrow[F]) extends Ops[F[A, B]] {
  ////
  final def ***[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.splitA(self, k)

  final def &&&[C](k: F[A, C]): F[A, (B, C)] =
    F.combine(self, k)

  final def product: F[(A, A), (B, B)] =
    F.product(self)

  ////
}

sealed trait ToArrowOpsU[TC[F[_, _]] <: Arrow[F]] {
  implicit def ToArrowOpsUnapply[FA](v: FA)(implicit F0: Unapply2[TC, FA]): ArrowOps[F0.M, F0.A, F0.B] =
    new ArrowOps[F0.M, F0.A, F0.B](F0(v))(using F0.TC)

}

trait ToArrowOps0[TC[F[_, _]] <: Arrow[F]] extends ToArrowOpsU[TC] {

  implicit def ToArrowOps[F[_, _],A, B](v: F[A, B])(implicit F0: TC[F]): ArrowOps[F, A, B] =
    new ArrowOps[F, A, B](v)


  implicit def ToArrowVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: TC[F[G, *, *]]): ArrowOps[F[G, *, *], A, B] =
    new ArrowOps[F[G, *, *], A, B](v)(using F0)

  ////

  ////
}

trait ToArrowOps[TC[F[_, _]] <: Arrow[F]] extends ToArrowOps0[TC] with ToSplitOps[TC] with ToStrongOps[TC] with ToCategoryOps[TC]

trait ArrowSyntax[F[_, _]] extends SplitSyntax[F] with StrongSyntax[F] with CategorySyntax[F] {
  implicit def ToArrowOps[A, B](v: F[A, B]): ArrowOps[F, A, B] = new ArrowOps[F, A, B](v)(using ArrowSyntax.this.F)

  def F: Arrow[F]
  ////

  ////
}
