package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BindRec` */
final class BindRecOps[F[_],A] private[syntax](val self: F[A])(implicit val F: BindRec[F]) extends Ops[F[A]] {
  ////

  def forever[B]: F[B] = F.forever(self)

  ////
}

sealed trait ToBindRecOps0 {
  implicit def ToBindRecOpsUnapply[FA](v: FA)(implicit F0: Unapply[BindRec, FA]): BindRecOps[F0.M, F0.A] =
    new BindRecOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToBindRecOps extends ToBindRecOps0 with ToBindOps {
  implicit def ToBindRecOps[F[_], A](v: F[A])(implicit F0: BindRec[F]): BindRecOps[F, A] =
    new BindRecOps[F, A](v)

  ////

  ////
}

trait BindRecSyntax[F[_]] extends BindSyntax[F] {
  implicit def ToBindRecOps[A](v: F[A]): BindRecOps[F, A] = new BindRecOps[F,A](v)(BindRecSyntax.this.F)

  def F: BindRec[F]
  ////

  ////
}
