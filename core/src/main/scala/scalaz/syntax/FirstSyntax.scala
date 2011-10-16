package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `First` */
trait FirstV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////

  ////
}

trait ToFirstSyntax  {
  implicit def ToFirstV[F[_, _],A, B](v: F[A, B]) =
    new FirstV[F,A, B] { def self = v }

  ////

  ////
}

trait FirstSyntax[F[_, _]]  {
  implicit def ToFirstV[A, B](v: F[A, B]): FirstV[F, A, B] = new FirstV[F, A, B] { def self = v }

  ////

  ////
}
