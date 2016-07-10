package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
final class CategoryOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Category[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToCategoryOps0 {
  implicit def ToCategoryOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Category, FA]) =
    new CategoryOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToCategoryOps extends ToCategoryOps0 with ToComposeOps {

  implicit def ToCategoryOps[F[_, _],A, B](v: F[A, B])(implicit F0: Category[F]) =
    new CategoryOps[F,A, B](v)


  implicit def ToCategoryVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Category[F[G, ?, ?]]) =
    new CategoryOps[F[G, ?, ?], A, B](v)(F0)

  ////
  ////
}

trait CategorySyntax[F[_, _]] extends ComposeSyntax[F] {
  implicit def ToCategoryOps[A, B](v: F[A, B]): CategoryOps[F, A, B] = new CategoryOps[F, A, B](v)(CategorySyntax.this.F)

  def F: Category[F]
  ////

  ////
}
