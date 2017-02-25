package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Compose` */
final class ComposeOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Compose[F]) extends Ops[F[A, B]] {
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

sealed trait ToComposeOps0 {
  implicit def ToComposeOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Compose, FA]) =
    new ComposeOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToComposeOps extends ToComposeOps0 {

  implicit def ToComposeOps[F[_, _],A, B](v: F[A, B])(implicit F0: Compose[F]) =
    new ComposeOps[F,A, B](v)


  implicit def ToComposeVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Compose[F[G, ?, ?]]) =
    new ComposeOps[F[G, ?, ?], A, B](v)(F0)

  ////
  ////
}

trait ComposeSyntax[F[_, _]]  {
  implicit def ToComposeOps[A, B](v: F[A, B]): ComposeOps[F, A, B] = new ComposeOps[F, A, B](v)(ComposeSyntax.this.F)

  def F: Compose[F]
  ////

  ////
}
