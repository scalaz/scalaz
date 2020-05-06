package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Unzip` */
final class UnzipOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Unzip[F]) extends Ops[F[A]] {
  ////
  ////
}

sealed trait ToUnzipOpsU[TC[F[_]] <: Unzip[F]] {
  implicit def ToUnzipOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): UnzipOps[F0.M, F0.A] =
    new UnzipOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToUnzipOps0[TC[F[_]] <: Unzip[F]] extends ToUnzipOpsU[TC] {
  implicit def ToUnzipOps[F[_],A](v: F[A])(implicit F0: TC[F]): UnzipOps[F, A] =
    new UnzipOps[F, A](v)

  ////
  implicit def ToUnzipPairOps[F[_],A,B](v: F[(A, B)])(implicit F0: TC[F]) =
    new UnzipPairOps[F,A,B](v)(F0)

  final class UnzipPairOps[F[_],A, B] private[syntax](self: F[(A, B)])(implicit F: TC[F]) {
    def unfzip: (F[A], F[B]) =
      F.unzip(self)

    def firsts: F[A] =
      F.firsts(self)

    def seconds: F[B] =
      F.seconds(self)
  }

  ////
}

trait ToUnzipOps[TC[F[_]] <: Unzip[F]] extends ToUnzipOps0[TC]

trait UnzipSyntax[F[_]]  {
  implicit def ToUnzipOps[A](v: F[A]): UnzipOps[F, A] = new UnzipOps[F,A](v)(UnzipSyntax.this.F)

  def F: Unzip[F]
  ////

  ////
}
