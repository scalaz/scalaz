package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
sealed abstract class CategoryOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Category[F]
  ////

  ////
}

trait ToCategoryOps0 {
    implicit def ToCategoryOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Category, FA]) =
      new CategoryOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Category[F0.M] = F0.TC }
  
}

trait ToCategoryOps extends ToCategoryOps0 with ToComposeOps {
  
  implicit def ToCategoryOps[F[_, _],A, B](v: F[A, B])(implicit F0: Category[F]) =
      new CategoryOps[F,A, B] { def self = v; implicit def F: Category[F] = F0 }
  

  ////
  implicit def ToCategoryVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Category[({type λ[α, β]=F[G, α, β]})#λ]) =
      new CategoryOps[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Category[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait CategorySyntax[F[_, _]] extends ComposeSyntax[F] {
  implicit def ToCategoryOps[A, B](v: F[A, B]): CategoryOps[F, A, B] = new CategoryOps[F, A, B] { def self = v; implicit def F: Category[F] = CategorySyntax.this.F }

  def F: Category[F]
  ////

  ////
}
