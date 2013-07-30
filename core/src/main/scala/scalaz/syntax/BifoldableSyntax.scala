package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifoldable` */
final class BifoldableOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Bifoldable[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToBifoldableOps0 {
    implicit def ToBifoldableOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bifoldable, FA]) =
      new BifoldableOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)
  
}

trait ToBifoldableOps extends ToBifoldableOps0 {
  
  implicit def ToBifoldableOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bifoldable[F]) =
      new BifoldableOps[F,A, B](v)
  

  ////

  ////
}

trait BifoldableSyntax[F[_, _]]  {
  implicit def ToBifoldableOps[A, B](v: F[A, B]): BifoldableOps[F, A, B] = new BifoldableOps[F, A, B](v)(BifoldableSyntax.this.F)

  def F: Bifoldable[F]
  ////

  ////
}
