package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `First` */
trait FirstV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: First[F]
  ////

  ////
}

trait ToFirstV  {
    implicit def ToFirstV[FA](v: FA)(implicit F0: Unapply2[First, FA]) =
      new FirstV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: First[F0.M] = F0.TC }
  

  ////

  ////
}

trait FirstSyntax[F[_, _]]  {
  implicit def ToFirstV[A, B](v: F[A, B])(implicit F0: First[F]): FirstV[F, A, B] = new FirstV[F, A, B] { def self = v; implicit def F: First[F] = F0 }

  ////

  ////
}
