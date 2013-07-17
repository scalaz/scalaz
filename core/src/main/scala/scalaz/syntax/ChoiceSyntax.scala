package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Choice` */
sealed abstract class ChoiceOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Choice[F]
  ////
  final def |||[C](x: => F[C, B]): F[A \/ C, B] =
    F.choice(self, x)
  ////
}

trait ToChoiceOps0 {
    implicit def ToChoiceOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Choice, FA]) =
      new ChoiceOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Choice[F0.M] = F0.TC }
  
}

trait ToChoiceOps extends ToChoiceOps0 with ToCategoryOps {
  
  implicit def ToChoiceOps[F[_, _],A, B](v: F[A, B])(implicit F0: Choice[F]) =
      new ChoiceOps[F,A, B] { def self = v; implicit def F: Choice[F] = F0 }
  

  ////
  implicit def ToComposeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Choice[({type λ[α, β]=F[G, α, β]})#λ]) =
        new ChoiceOps[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Choice[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait ChoiceSyntax[F[_, _]] extends CategorySyntax[F] {
  implicit def ToChoiceOps[A, B](v: F[A, B]): ChoiceOps[F, A, B] = new ChoiceOps[F, A, B] { def self = v; implicit def F: Choice[F] = ChoiceSyntax.this.F }

  def F: Choice[F]
  ////

  ////
}
