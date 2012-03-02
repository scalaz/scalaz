package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Category` */
trait ChoiceSyntaxV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Choice[F]

  ////
  final def |||[C](x: => F[C, B]): F[Either[A, C], B] =
    F.choice(self, x)
  ////

}

trait ToChoiceV0 {
    implicit def ToChoiceVUnapply[FA](v: FA)(implicit F0: Unapply2[Choice, FA]) =
      new ChoiceSyntaxV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Choice[F0.M] = F0.TC }

}

trait ToChoiceV extends ToChoiceV0 {

  implicit def ToChoiceV[F[_, _],A, B](v: F[A, B])(implicit F0: Choice[F]) =
      new ChoiceSyntaxV[F,A, B] { def self = v; implicit def F: Choice[F] = F0 }


  ////
  implicit def ToComposeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Choice[({type λ[α, β]=F[G, α, β]})#λ]) =
        new ChoiceSyntaxV[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Choice[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait ChoiceSyntax[F[_, _]]  {
  implicit def ToChoiceV[A, B](v: F[A, B])(implicit F0: Choice[F]): ChoiceSyntaxV[F, A, B] = new ChoiceSyntaxV[F, A, B] { def self = v; implicit def F: Choice[F] = F0 }

  ////

  ////
}
