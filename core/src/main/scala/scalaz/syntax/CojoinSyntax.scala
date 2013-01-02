package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cojoin` */
trait CojoinOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Cojoin[F]
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  ////
}

trait ToCojoinOps0 {
  implicit def ToCojoinOpsUnapply[FA](v: FA)(implicit F0: Unapply[Cojoin, FA]) =
    new CojoinOps[F0.M,F0.A] { def self = F0(v); implicit def F: Cojoin[F0.M] = F0.TC }

}

trait ToCojoinOps extends ToCojoinOps0 with ToFunctorOps {
  implicit def ToCojoinOps[F[_],A](v: F[A])(implicit F0: Cojoin[F]) =
    new CojoinOps[F,A] { def self = v; implicit def F: Cojoin[F] = F0 }

  ////

  ////
}

trait CojoinSyntax[F[_]] extends FunctorSyntax[F] {
  implicit def ToCojoinOps[A](v: F[A]): CojoinOps[F, A] = new CojoinOps[F,A] { def self = v; implicit def F: Cojoin[F] = CojoinSyntax.this.F }

  def F: Cojoin[F]
  ////

  ////
}
