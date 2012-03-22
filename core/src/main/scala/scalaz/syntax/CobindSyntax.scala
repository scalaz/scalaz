package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cobind` */
trait CobindV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Cobind[F]
  ////
  def cobind[B](f: F[A] => B) = F.cobind(self)(f)
  ////
}

trait ToCobindV0 {
  implicit def ToCobindVUnapply[FA](v: FA)(implicit F0: Unapply[Cobind, FA]) =
    new CobindV[F0.M,F0.A] { def self = F0(v); implicit def F: Cobind[F0.M] = F0.TC }

}

trait ToCobindV extends ToCobindV0 {
  implicit def ToCobindV[F[_],A](v: F[A])(implicit F0: Cobind[F]) =
    new CobindV[F,A] { def self = v; implicit def F: Cobind[F] = F0 }

  ////

  ////
}

trait CobindSyntax[F[_]]  {
  implicit def ToCobindV[A](v: F[A])(implicit F0: Cobind[F]): CobindV[F, A] = new CobindV[F,A] { def self = v; implicit def F: Cobind[F] = F0 }

  ////

  ////
}
