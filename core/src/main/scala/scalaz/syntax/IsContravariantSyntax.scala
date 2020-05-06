package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `IsContravariant` */
final class IsContravariantOps[F[_],A] private[syntax](val self: F[A])(implicit val F: IsContravariant[F]) extends Ops[F[A]] {
  ////
  import Liskov._

  def narrow[B](implicit ev: A >~> B): F[B] = F.narrow(self)

  ////
}

sealed trait ToIsContravariantOpsU[TC[F[_]] <: IsContravariant[F]] {
  implicit def ToIsContravariantOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): IsContravariantOps[F0.M, F0.A] =
    new IsContravariantOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToIsContravariantOps0[TC[F[_]] <: IsContravariant[F]] extends ToIsContravariantOpsU[TC] {
  implicit def ToIsContravariantOps[F[_],A](v: F[A])(implicit F0: TC[F]): IsContravariantOps[F, A] =
    new IsContravariantOps[F, A](v)

  ////

  ////
}

trait ToIsContravariantOps[TC[F[_]] <: IsContravariant[F]] extends ToIsContravariantOps0[TC]

trait IsContravariantSyntax[F[_]]  {
  implicit def ToIsContravariantOps[A](v: F[A]): IsContravariantOps[F, A] = new IsContravariantOps[F,A](v)(IsContravariantSyntax.this.F)

  def F: IsContravariant[F]
  ////

  ////
}
