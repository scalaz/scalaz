package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cobind` */
final class CobindOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Cobind[F]) extends Ops[F[A]] {
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  final def coflatten: F[F[A]] = F.cojoin(self)
  final def cobind[B](f: F[A] => B): F[B] = F.cobind(self)(f)
  final def coflatMap[B](f: F[A] => B): F[B] = F.cobind(self)(f)
  ////
}

sealed trait ToCobindOpsU[TC[F[_]] <: Cobind[F]] {
  implicit def ToCobindOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): CobindOps[F0.M, F0.A] =
    new CobindOps[F0.M, F0.A](F0(v))(using F0.TC)

}

trait ToCobindOps0[TC[F[_]] <: Cobind[F]] extends ToCobindOpsU[TC] {
  implicit def ToCobindOps[F[_],A](v: F[A])(implicit F0: TC[F]): CobindOps[F, A] =
    new CobindOps[F, A](v)

  ////

  ////
}

trait ToCobindOps[TC[F[_]] <: Cobind[F]] extends ToCobindOps0[TC] with ToFunctorOps[TC]

trait CobindSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCobindOps[A](v: F[A]): CobindOps[F, A] = new CobindOps[F,A](v)(using CobindSyntax.this.F)

  def F: Cobind[F]
  ////

  ////
}
