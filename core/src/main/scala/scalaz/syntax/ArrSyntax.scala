package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arr` */
trait ArrV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
}

trait ToArrSyntax  {
  implicit def ToArrV[F[_, _],A, B](v: F[A, B]) =
    new ArrV[F,A,B] { def self = v }

}

trait ArrSyntax[F[_, _]]  {
  implicit def ToArrV[A, B](v: F[A, B]): ArrV[F, A, B] = new ArrV[F,A,B] { def self = v }

}
