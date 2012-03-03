package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
trait SplitSyntaxV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Split[F]

  ////
  final def -*-[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.split(self, k)
  ////

}

trait ToSplitV0 {
    implicit def ToSplitVUnapply[FA](v: FA)(implicit F0: Unapply2[Split, FA]) =
      new SplitSyntaxV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Split[F0.M] = F0.TC }

}

trait ToSplitV extends ToSplitV0 {

  implicit def ToSplitV[F[_, _],A, B](v: F[A, B])(implicit F0: Split[F]) =
      new SplitSyntaxV[F,A, B] { def self = v; implicit def F: Split[F] = F0 }


  ////
  implicit def ToComposeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Split[({type λ[α, β]=F[G, α, β]})#λ]) =
        new SplitSyntaxV[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Split[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait SplitSyntax[F[_, _]]  {
  implicit def ToSplitV[A, B](v: F[A, B])(implicit F0: Split[F]): SplitSyntaxV[F, A, B] = new SplitSyntaxV[F, A, B] { def self = v; implicit def F: Split[F] = F0 }

  ////

  ////
}
