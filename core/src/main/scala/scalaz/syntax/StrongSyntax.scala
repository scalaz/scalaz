package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Strong` */
final class StrongOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Strong[F]) extends Ops[F[A, B]] {
  ////
  final def first[C]: F[(A, C), (B, C)] =
    F.first(self)

  final def second[C]: F[(C, A), (C, B)] =
    F.second(self)

  ////
}

sealed trait ToStrongOps0 {
  implicit def ToStrongOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Strong, FA]) =
    new StrongOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToStrongOps extends ToStrongOps0 with ToProfunctorOps {

  implicit def ToStrongOps[F[_, _],A, B](v: F[A, B])(implicit F0: Strong[F]) =
    new StrongOps[F,A, B](v)


  implicit def ToStrongVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Strong[F[G, ?, ?]]) =
    new StrongOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait StrongSyntax[F[_, _]] extends ProfunctorSyntax[F] {
  implicit def ToStrongOps[A, B](v: F[A, B]): StrongOps[F, A, B] = new StrongOps[F, A, B](v)(StrongSyntax.this.F)

  def F: Strong[F]
  ////

  ////
}
