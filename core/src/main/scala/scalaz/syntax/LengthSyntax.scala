package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Length` */
trait LengthV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Length[F]
  ////
  final def length: Int = F.length(self)
  ////
}

trait ToLengthV  {
  implicit def ToLengthV[FA](v: FA)(implicit F0: Unapply[Length, FA]) =
    new LengthV[F0.M,F0.A] { def self = F0(v); implicit def F: Length[F0.M] = F0.TC }

  ////

  ////
}

trait LengthSyntax[F[_]]  {
  implicit def ToLengthV[A](v: F[A])(implicit F0: Length[F]): LengthV[F, A] = new LengthV[F,A] { def self = v; implicit def F: Length[F] = F0 }

  ////

  ////
}
