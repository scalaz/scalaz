package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Unzip` */
final class UnzipOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Unzip[F]) extends Ops[F[A]] {
  ////
  ////
}

sealed trait ToUnzipOps0 {
  implicit def ToUnzipOpsUnapply[FA](v: FA)(implicit F0: Unapply[Unzip, FA]): UnzipOps[F0.M, F0.A] =
    new UnzipOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToUnzipOps extends ToUnzipOps0 {
  implicit def ToUnzipOps[F[_], A](v: F[A])(implicit F0: Unzip[F]): UnzipOps[F, A] =
    new UnzipOps[F, A](v)

  ////
  implicit def ToUnzipPairOps[F[_],A,B](v: F[(A, B)])(implicit F0: Unzip[F]): UnzipPairOps[F, A, B] =
    new UnzipPairOps[F,A,B](v)(F0)

  final class UnzipPairOps[F[_],A, B] private[syntax](self: F[(A, B)])(implicit F: Unzip[F]) {
    def unfzip: (F[A], F[B]) =
      F.unzip(self)

    def firsts: F[A] =
      F.firsts(self)

    def seconds: F[B] =
      F.seconds(self)
  }

  ////
}

trait UnzipSyntax[F[_]]  {
  implicit def ToUnzipOps[A](v: F[A]): UnzipOps[F, A] = new UnzipOps[F,A](v)(UnzipSyntax.this.F)

  def F: Unzip[F]
  ////

  ////
}
