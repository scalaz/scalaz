package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Associative` */
final class AssociativeOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Associative[F]) extends Ops[F[A, B]] {
  ////
  import Leibniz.===

  final def reassociateLeft[TT, C](implicit ev: B === F[TT, C]): F[F[A, TT], C] =
    F.reassociateLeft(ev.subst[F[A, ?]](self))

  final def reassociateRight[TT, C](implicit ev: A === F[TT, C]): F[TT, F[C, B]] =
    F.reassociateRight(ev.subst[F[?, B]](self))

  ////
}

sealed trait ToAssociativeOps0 {
  implicit def ToAssociativeOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Associative, FA]) =
    new AssociativeOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToAssociativeOps extends ToAssociativeOps0 {

  implicit def ToAssociativeOps[F[_, _],A, B](v: F[A, B])(implicit F0: Associative[F]) =
    new AssociativeOps[F,A, B](v)


  implicit def ToAssociativeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Associative[F[G, ?, ?]]) =
    new AssociativeOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait AssociativeSyntax[F[_, _]]  {
  implicit def ToAssociativeOps[A, B](v: F[A, B]): AssociativeOps[F, A, B] = new AssociativeOps[F, A, B](v)(AssociativeSyntax.this.F)

  def F: Associative[F]
  ////

  ////
}
