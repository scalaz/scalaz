package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
trait CategoryV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////

  ////
}

trait ToCategorySyntax extends ToArrIdSyntax with ToComposeSyntax {
  implicit def ToCategoryV[F[_, _],A, B](v: F[A, B]) =
    new CategoryV[F,A, B] { def self = v }

  ////

  ////
}

trait CategorySyntax[F[_, _]] extends ArrIdSyntax[F] with ComposeSyntax[F] {
  implicit def ToCategoryV[A, B](v: F[A, B]): CategoryV[F, A, B] = new CategoryV[F, A, B] { def self = v }

  ////

  ////
}
