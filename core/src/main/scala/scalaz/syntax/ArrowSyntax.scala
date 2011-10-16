package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arrow` */
trait ArrowV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////

  ////
}

trait ToArrowSyntax extends ToCategorySyntax with ToArrSyntax with ToFirstSyntax {
  implicit def ToArrowV[F[_, _],A, B](v: F[A, B]) =
    new ArrowV[F,A, B] { def self = v }

  ////

  ////
}

trait ArrowSyntax[F[_, _]] extends CategorySyntax[F] with ArrSyntax[F] with FirstSyntax[F] {
  implicit def ToArrowV[A, B](v: F[A, B]): ArrowV[F, A, B] = new ArrowV[F, A, B] { def self = v }

  ////

  ////
}
