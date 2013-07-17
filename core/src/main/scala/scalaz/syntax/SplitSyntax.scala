package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Split` */
sealed abstract class SplitOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Split[F]
  ////
  final def -*-[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.split(self, k)
  ////
}

trait ToSplitOps0 {
    implicit def ToSplitOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Split, FA]) =
      new SplitOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Split[F0.M] = F0.TC }
  
}

trait ToSplitOps extends ToSplitOps0 with ToComposeOps {
  
  implicit def ToSplitOps[F[_, _],A, B](v: F[A, B])(implicit F0: Split[F]) =
      new SplitOps[F,A, B] { def self = v; implicit def F: Split[F] = F0 }
  

  ////
  implicit def ToComposeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Split[({type λ[α, β]=F[G, α, β]})#λ]) =
        new SplitOps[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Split[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait SplitSyntax[F[_, _]] extends ComposeSyntax[F] {
  implicit def ToSplitOps[A, B](v: F[A, B]): SplitOps[F, A, B] = new SplitOps[F, A, B] { def self = v; implicit def F: Split[F] = SplitSyntax.this.F }

  def F: Split[F]
  ////

  ////
}
