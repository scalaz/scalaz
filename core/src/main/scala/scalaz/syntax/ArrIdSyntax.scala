package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ArrId` */
trait ArrIdV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////

  ////
}

trait ToArrIdSyntax  {
  implicit def ToArrIdV[F[_, _],A, B](v: F[A, B]) =
    new ArrIdV[F,A, B] { def self = v }

  ////

  ////
}

trait ArrIdSyntax[F[_, _]]  {
  implicit def ToArrIdV[A, B](v: F[A, B]): ArrIdV[F, A, B] = new ArrIdV[F, A, B] { def self = v }

  ////

  ////
}
