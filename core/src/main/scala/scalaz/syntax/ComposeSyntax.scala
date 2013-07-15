package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Compose` */
sealed abstract class ComposeOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Compose[F]
  ////
  final def <<<[C](x: F[C, A]): F[C, B] =
    F.compose(self, x)

  final def ⋘[C](x: F[C, A]): F[C, B] =
    <<<(x)

  final def >>>[C](x: F[B, C]): F[A, C] =
    F.compose(x, self)

  final def ⋙[C](x: F[B, C]): F[A, C] =
    >>>(x)
  ////
}

trait ToComposeOps0 {
    implicit def ToComposeOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Compose, FA]) =
      new ComposeOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Compose[F0.M] = F0.TC }
  
}

trait ToComposeOps extends ToComposeOps0 {
  
  implicit def ToComposeOps[F[_, _],A, B](v: F[A, B])(implicit F0: Compose[F]) =
      new ComposeOps[F,A, B] { def self = v; implicit def F: Compose[F] = F0 }
  

  ////
  // TODO Roll this back into gen-type-class to add to all type classes classifying * * -> *
  implicit def ToComposeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Compose[({type λ[α, β]=F[G, α, β]})#λ]) =
        new ComposeOps[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Compose[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait ComposeSyntax[F[_, _]]  {
  implicit def ToComposeOps[A, B](v: F[A, B]): ComposeOps[F, A, B] = new ComposeOps[F, A, B] { def self = v; implicit def F: Compose[F] = ComposeSyntax.this.F }

  def F: Compose[F]
  ////

  ////
}
