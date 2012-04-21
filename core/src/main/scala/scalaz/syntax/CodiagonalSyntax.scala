package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Codiagonal` */
trait CodiagonalOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Codiagonal[F]
  ////

  ////
}

trait ToCodiagonalOps0 {
    implicit def ToCodiagonalOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Codiagonal, FA]) =
      new CodiagonalOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Codiagonal[F0.M] = F0.TC }
  
}

trait ToCodiagonalOps extends ToCodiagonalOps0 {
  
  implicit def ToCodiagonalOps[F[_, _],A, B](v: F[A, B])(implicit F0: Codiagonal[F]) =
      new CodiagonalOps[F,A, B] { def self = v; implicit def F: Codiagonal[F] = F0 }
  

  ////

  ////
}

trait CodiagonalSyntax[F[_, _]]  {
  implicit def ToCodiagonalOps[A, B](v: F[A, B])(implicit F0: Codiagonal[F]): CodiagonalOps[F, A, B] = new CodiagonalOps[F, A, B] { def self = v; implicit def F: Codiagonal[F] = F0 }

  ////

  ////
}
