package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arr` */
trait ArrV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Arr[F]
  ////

  ////
}

trait ToArrV  {
    implicit def ToArrV[FA](v: FA)(implicit F0: Unapply2[Arr, FA]) =
      new ArrV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Arr[F0.M] = F0.TC }
  

  ////

  ////
}

trait ArrSyntax[F[_, _]]  {
  implicit def ToArrV[A, B](v: F[A, B])(implicit F0: Arr[F]): ArrV[F, A, B] = new ArrV[F, A, B] { def self = v; implicit def F: Arr[F] = F0 }

  ////

  ////
}
