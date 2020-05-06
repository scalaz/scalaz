package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cobind` */
final class CobindOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Cobind[F]) extends Ops[F[A]] {
  ////
  def cojoin: F[F[A]] = F.cojoin(self)
  def coflatten: F[F[A]] = F.cojoin(self)
  def duplicate: F[F[A]] = F.cojoin(self)

  def cobind[B](f: F[A] => B): F[B] = F.cobind(self)(f)
  def extend[B](f: F[A] => B): F[B] = F.cobind(self)(f)
  def coflatMap[B](f: F[A] => B): F[B] = F.cobind(self)(f)
  ////
}

sealed trait ToCobindOps0 {
  implicit def ToCobindOpsUnapply[FA](v: FA)(implicit F0: Unapply[Cobind, FA]): CobindOps[F0.M, F0.A] =
    new CobindOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToCobindOps extends ToCobindOps0 with ToFunctorOps {
  implicit def ToCobindOps[F[_], A](v: F[A])(implicit F0: Cobind[F]): CobindOps[F, A] =
    new CobindOps[F, A](v)

  ////

  ////
}

trait CobindSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCobindOps[A](v: F[A]): CobindOps[F, A] = new CobindOps[F,A](v)(CobindSyntax.this.F)

  def F: Cobind[F]
  ////

  ////
}
