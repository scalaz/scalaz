package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
trait ComonadOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Comonad[F]
  ////
  def copoint: A = F.copoint(self)

  ////
}

trait ToComonadOps0 {
  implicit def ToComonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[Comonad, FA]) =
    new ComonadOps[F0.M,F0.A] { def self = F0(v); implicit def F: Comonad[F0.M] = F0.TC }

}

trait ToComonadOps extends ToComonadOps0 with ToCojoinOps with ToCobindOps {
  implicit def ToComonadOps[F[_],A](v: F[A])(implicit F0: Comonad[F]) =
    new ComonadOps[F,A] { def self = v; implicit def F: Comonad[F] = F0 }

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CojoinSyntax[F] with CobindSyntax[F] {
  implicit def ToComonadOps[A](v: F[A]): ComonadOps[F, A] = new ComonadOps[F,A] { def self = v; implicit def F: Comonad[F] = ComonadSyntax.this.F }

  def F: Comonad[F]
  ////

  ////
}
