package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Compose` */
trait ComposeV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Compose[F]
  ////
  def <<<[C](x: F[C, A]): F[C, B] =
    F.compose(self, x)

  def ⋘[C](x: F[C, A]): F[C, B] =
    <<<(x)

  def >>>[C](x: F[B, C]): F[A, C] =
    F.compose(x, self)

  def ⋙[C](x: F[B, C]): F[A, C] =
    >>>(x)
  ////
}

trait ToComposeV  {
  implicit def ToComposeV[F[_, _],A, B](v: F[A, B])(implicit F0: Compose[F]) =
    new ComposeV[F,A, B] { def self = v; implicit def F: Compose[F] = F0 }

  ////
  // TODO Roll this back into gen-type-class to add to all type classes classifying * * -> *
  implicit def ToComposeVFromKliesliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Compose[({type λ[α, β]=F[G, α, β]})#λ]) =
        new ComposeV[({type λ[α, β]=F[G, α, β]})#λ, A, B] { def self = v; implicit def F: Compose[({type λ[α, β]=F[G, α, β]})#λ] = F0 }

  ////
}

trait ComposeSyntax[F[_, _]]  {
  implicit def ToComposeV[A, B](v: F[A, B])(implicit F0: Compose[F]): ComposeV[F, A, B] = new ComposeV[F, A, B] { def self = v; implicit def F: Compose[F] = F0 }

  ////

  ////
}
