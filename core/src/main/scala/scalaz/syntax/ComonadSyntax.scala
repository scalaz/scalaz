package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
trait ComonadV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Comonad[F]
  ////

  ////
}

trait ToComonadV0 {
  implicit def ToComonadVUnapply[FA](v: FA)(implicit F0: Unapply[Comonad, FA]) =
    new ComonadV[F0.M,F0.A] { def self = F0(v); implicit def F: Comonad[F0.M] = F0.TC }

}

trait ToComonadV extends ToComonadV0 with ToCopointedV with ToCojoinV with ToCobindV {
  implicit def ToComonadV[F[_],A](v: F[A])(implicit F0: Comonad[F]) =
    new ComonadV[F,A] { def self = v; implicit def F: Comonad[F] = F0 }

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CopointedSyntax[F] with CojoinSyntax[F] with CobindSyntax[F] {
  implicit def ToComonadV[A](v: F[A])(implicit F0: Comonad[F]): ComonadV[F, A] = new ComonadV[F,A] { def self = v; implicit def F: Comonad[F] = F0 }

  ////

  ////
}
