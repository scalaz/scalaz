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

trait ToComposeSyntax  {
  implicit def ToComposeV[F[_, _],A, B](v: F[A, B])(implicit F0: Compose[F]) =
    new ComposeV[F,A, B] { def self = v; implicit def F: Compose[F] = F0 }

  ////

  ////
}

trait ComposeSyntax[F[_, _]]  {
  implicit def ToComposeV[A, B](v: F[A, B])(implicit F0: Compose[F]): ComposeV[F, A, B] = new ComposeV[F, A, B] { def self = v; implicit def F: Compose[F] = F0 }

  ////

  ////
}
