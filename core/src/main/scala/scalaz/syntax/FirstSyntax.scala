package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `First` */
trait FirstV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: First[F]
  ////

  ////
}

trait ToFirstV  {
  implicit def ToFirstV[F[_, _],A, B](v: F[A, B])(implicit F0: First[F]) =
    new FirstV[F,A, B] { def self = v; implicit def F: First[F] = F0 }

  ////

  ////
}

trait FirstSyntax[F[_, _]]  {
  implicit def ToFirstV[A, B](v: F[A, B])(implicit F0: First[F]): FirstV[F, A, B] = new FirstV[F, A, B] { def self = v; implicit def F: First[F] = F0 }

  ////

  ////
}
