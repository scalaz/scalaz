package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Unzip` */
final class UnzipOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Unzip[F]) extends Ops[F[A]] {
  ////
  ////
}

sealed trait ToUnzipOps0 {
  implicit def ToUnzipOpsUnapply[FA](v: FA)(implicit F0: Unapply[Unzip, FA]) =
    new UnzipOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToUnzipOps extends ToUnzipOps0 {
  implicit def ToUnzipOps[F[_],A](v: F[A])(implicit F0: Unzip[F]) =
    new UnzipOps[F,A](v)

  ////
  implicit def ToUnzipPairOps[F[_],A,B](v: F[(A, B)])(implicit F0: Unzip[F]) =
    new UnzipPairOps[F,A,B] { def self = v; implicit def F: Unzip[F] = F0 }

  trait UnzipPairOps[F[_],A, B] extends Ops[F[(A, B)]] {
    implicit def F: Unzip[F]
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
