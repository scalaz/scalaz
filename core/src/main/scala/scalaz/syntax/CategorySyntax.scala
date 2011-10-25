package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
trait CategoryV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Category[F]
  ////

  ////
}

trait ToCategoryV extends ToArrIdV with ToComposeV {
  implicit def ToCategoryV[F[_, _],A, B](v: F[A, B])(implicit F0: Category[F]) =
    new CategoryV[F,A, B] { def self = v; implicit def F: Category[F] = F0 }

  ////

  ////
}

trait CategorySyntax[F[_, _]] extends ArrIdSyntax[F] with ComposeSyntax[F] {
  implicit def ToCategoryV[A, B](v: F[A, B])(implicit F0: Category[F]): CategoryV[F, A, B] = new CategoryV[F, A, B] { def self = v; implicit def F: Category[F] = F0 }

  ////

  ////
}
