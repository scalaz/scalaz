package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BindRec` */
final class BindRecOps[F[_],A] private[syntax](val self: F[A])(implicit val F: BindRec[F]) extends Ops[F[A]] {
  ////

  def forever[B]: F[B] = F.forever(self)

  ////
}

sealed trait ToBindRecOpsU[TC[F[_]] <: BindRec[F]] {
  implicit def ToBindRecOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]) =
    new BindRecOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToBindRecOps0[TC[F[_]] <: BindRec[F]] extends ToBindRecOpsU[TC] {
  implicit def ToBindRecOps[F[_],A](v: F[A])(implicit F0: TC[F]) =
    new BindRecOps[F,A](v)

  ////

  ////
}

trait ToBindRecOps[TC[F[_]] <: BindRec[F]] extends ToBindRecOps0[TC] with ToBindOps[TC]

trait BindRecSyntax[F[_]] extends BindSyntax[F] {
  implicit def ToBindRecOps[A](v: F[A]): BindRecOps[F, A] = new BindRecOps[F,A](v)(BindRecSyntax.this.F)

  def F: BindRec[F]
  ////

  ////
}
