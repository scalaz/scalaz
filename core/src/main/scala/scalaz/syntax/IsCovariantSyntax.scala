package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `IsCovariant` */
final class IsCovariantOps[F[_],A] private[syntax](val self: F[A])(implicit val F: IsCovariant[F]) extends Ops[F[A]] {
  ////
  import Liskov._

  def widen[B](implicit ev: A <~< B): F[B] = F.widen(self)

  ////
}

sealed trait ToIsCovariantOpsU[TC[F[_]] <: IsCovariant[F]] {
  implicit def ToIsCovariantOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new IsCovariantOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToIsCovariantOps0[TC[F[_]] <: IsCovariant[F]] extends ToIsCovariantOpsU[TC] {
  implicit def ToIsCovariantOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new IsCovariantOps[F,A](v)

  ////

  ////
}

trait ToIsCovariantOps[TC[F[_]] <: IsCovariant[F]] extends ToIsCovariantOps0[TC]

trait IsCovariantSyntax[F[_]]  {
  implicit def ToIsCovariantOps[A](v: F[A]): IsCovariantOps[F, A] = new IsCovariantOps[F,A](v)(IsCovariantSyntax.this.F)

  def F: IsCovariant[F]
  ////

  ////
}
