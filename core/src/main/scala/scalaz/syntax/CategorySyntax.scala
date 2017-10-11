package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
final class CategoryOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Category[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToCategoryOpsU[TC[F[_, _]] <: Category[F]] {
  implicit def ToCategoryOpsUnapply[FA](v: FA)(implicit F0: Unapply2[TC, FA]) =
    new CategoryOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToCategoryOps0[TC[F[_, _]] <: Category[F]] extends ToCategoryOpsU[TC] {

  implicit def ToCategoryOps[F[_, _],A, B](v: F[A, B])(implicit F0: TC[F]) =
    new CategoryOps[F,A, B](v)


  implicit def ToCategoryVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: TC[F[G, ?, ?]]) =
    new CategoryOps[F[G, ?, ?], A, B](v)(F0)

  ////
  ////
}

trait ToCategoryOps[TC[F[_, _]] <: Category[F]] extends ToCategoryOps0[TC] with ToComposeOps[TC]

trait CategorySyntax[F[_, _]] extends ComposeSyntax[F] {
  implicit def ToCategoryOps[A, B](v: F[A, B]): CategoryOps[F, A, B] = new CategoryOps[F, A, B](v)(CategorySyntax.this.F)

  def F: Category[F]
  ////

  ////
}
