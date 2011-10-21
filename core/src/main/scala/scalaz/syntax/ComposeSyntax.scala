package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Compose` */
trait ComposeV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////
  def <<<[C](x: F[C, A])(implicit c: Compose[F]): F[C, B] =
    c.compose(self, x)

  def ⋘[C](x: F[C, A])(implicit c: Compose[F]): F[C, B] =
    <<<(x)

  def >>>[C](x: F[B, C])(implicit c: Compose[F]): F[A, C] =
    c.compose(x, self)

  def ⋙[C](x: F[B, C])(implicit c: Compose[F]): F[A, C] =
    >>>(x)
  ////
}

trait ToComposeSyntax  {
  implicit def ToComposeV[F[_, _],A, B](v: F[A, B]) =
    new ComposeV[F,A, B] { def self = v }

  ////

  ////
}

trait ComposeSyntax[F[_, _]]  {
  implicit def ToComposeV[A, B](v: F[A, B]): ComposeV[F, A, B] = new ComposeV[F, A, B] { def self = v }

  ////

  ////
}
