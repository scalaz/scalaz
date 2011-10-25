package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arr` */
trait ArrV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Arr[F]
  ////

  ////
}

trait ToArrV  {
  implicit def ToArrV[F[_, _],A, B](v: F[A, B])(implicit F0: Arr[F]) =
    new ArrV[F,A, B] { def self = v; implicit def F: Arr[F] = F0 }

  ////

  ////
}

trait ArrSyntax[F[_, _]]  {
  implicit def ToArrV[A, B](v: F[A, B])(implicit F0: Arr[F]): ArrV[F, A, B] = new ArrV[F, A, B] { def self = v; implicit def F: Arr[F] = F0 }

  ////

  ////
}
