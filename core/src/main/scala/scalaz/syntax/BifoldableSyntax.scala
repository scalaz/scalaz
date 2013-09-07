package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifoldable` */
sealed abstract class BifoldableOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Bifoldable[F]
  ////

  ////
}

sealed trait ToBifoldableOps0 {
    implicit def ToBifoldableOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bifoldable, FA]) =
      new BifoldableOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Bifoldable[F0.M] = F0.TC }
  
}

trait ToBifoldableOps extends ToBifoldableOps0 {
  
  implicit def ToBifoldableOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bifoldable[F]) =
      new BifoldableOps[F,A, B] { def self = v; implicit def F: Bifoldable[F] = F0 }
  

  
  implicit def ToBifoldableVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Bifoldable[({type λ[α, β]=F[G, α, β]})#λ]) =
        new BifoldableOps[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Bifoldable[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////

  ////
}

trait BifoldableSyntax[F[_, _]]  {
  implicit def ToBifoldableOps[A, B](v: F[A, B]): BifoldableOps[F, A, B] = new BifoldableOps[F, A, B] { def self = v; implicit def F: Bifoldable[F] = BifoldableSyntax.this.F }

  def F: Bifoldable[F]
  ////

  ////
}
