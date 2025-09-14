package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Nondeterminism` */
final class NondeterminismOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Nondeterminism[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToNondeterminismOpsU[TC[F[_]] <: Nondeterminism[F]] {
  implicit def ToNondeterminismOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): NondeterminismOps[F0.M, F0.A] =
    new NondeterminismOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToNondeterminismOps0[TC[F[_]] <: Nondeterminism[F]] extends ToNondeterminismOpsU[TC] {
  implicit def ToNondeterminismOps[F[_],A](v: F[A])(implicit F0: TC[F]): NondeterminismOps[F, A] =
    new NondeterminismOps[F, A](v)

  ////

  ////
}

trait ToNondeterminismOps[TC[F[_]] <: Nondeterminism[F]] extends ToNondeterminismOps0[TC] with ToMonadOps[TC]

trait NondeterminismSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToNondeterminismOps[A](v: F[A]): NondeterminismOps[F, A] = new NondeterminismOps[F,A](v)(using NondeterminismSyntax.this.F)

  def F: Nondeterminism[F]
  ////

  ////
}
