package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Associative` */
final class AssociativeOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Associative[F]) extends Ops[F[A, B]] {
  ////
  import Liskov._

  final def reassociateLeft[TT, C](implicit ev: F[A, B] <~< F[A, F[TT, C]]): F[F[A, TT], C] =
    F.reassociateLeft(ev(self))

  final def reassociateRight[TT, C](implicit ev: F[A, B] <~< F[F[TT, C], B]): F[TT, F[C, B]] =
    F.reassociateRight(ev(self))

  ////
}

sealed trait ToAssociativeOpsU[TC[F[_, _]] <: Associative[F]] {
  implicit def ToAssociativeOpsUnapply[FA](v: FA)(implicit F0: Unapply2[TC, FA]) =
    new AssociativeOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToAssociativeOps0[TC[F[_, _]] <: Associative[F]] extends ToAssociativeOpsU[TC] {

  implicit def ToAssociativeOps[F[_, _],A, B](v: F[A, B])(implicit F0: TC[F]) =
    new AssociativeOps[F,A, B](v)


  implicit def ToAssociativeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: TC[F[G, ?, ?]]) =
    new AssociativeOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ToAssociativeOps[TC[F[_, _]] <: Associative[F]] extends ToAssociativeOps0[TC]

trait AssociativeSyntax[F[_, _]]  {
  implicit def ToAssociativeOps[A, B](v: F[A, B]): AssociativeOps[F, A, B] = new AssociativeOps[F, A, B](v)(AssociativeSyntax.this.F)

  def F: Associative[F]
  ////

  ////
}
