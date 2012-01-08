package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Plus` */
trait PlusV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Plus[F]
  ////

  final def <+>(other: => F[A]) = F.plus(self, other)

  ////
}

trait ToPlusV0 {
  implicit def ToPlusVUnapply[FA](v: FA)(implicit F0: Unapply[Plus, FA]) =
    new PlusV[F0.M,F0.A] { def self = F0(v); implicit def F: Plus[F0.M] = F0.TC }

}

trait ToPlusV extends ToPlusV0 {
  implicit def ToPlusV[F[_],A](v: F[A])(implicit F0: Plus[F]) =
    new PlusV[F,A] { def self = v; implicit def F: Plus[F] = F0 }

  ////

  ////
}

trait PlusSyntax[F[_]]  {
  implicit def ToPlusV[A](v: F[A])(implicit F0: Plus[F]): PlusV[F, A] = new PlusV[F,A] { def self = v; implicit def F: Plus[F] = F0 }

  ////

  ////
}
