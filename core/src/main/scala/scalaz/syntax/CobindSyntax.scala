package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cobind` */
sealed abstract class CobindOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Cobind[F]
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  final def coflatten: F[F[A]] = F.cojoin(self)
  final def cobind[B](f: F[A] => B) = F.cobind(self)(f)
  final def coflatMap[B](f: F[A] => B) = F.cobind(self)(f)
  ////
}

trait ToCobindOps0 {
  implicit def ToCobindOpsUnapply[FA](v: FA)(implicit F0: Unapply[Cobind, FA]) =
    new CobindOps[F0.M,F0.A] { def self = F0(v); implicit def F: Cobind[F0.M] = F0.TC }

}

trait ToCobindOps extends ToCobindOps0 with ToFunctorOps {
  implicit def ToCobindOps[F[_],A](v: F[A])(implicit F0: Cobind[F]) =
    new CobindOps[F,A] { def self = v; implicit def F: Cobind[F] = F0 }

  ////

  ////
}

trait CobindSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCobindOps[A](v: F[A]): CobindOps[F, A] = new CobindOps[F,A] { def self = v; implicit def F: Cobind[F] = CobindSyntax.this.F }

  def F: Cobind[F]
  ////

  ////
}
