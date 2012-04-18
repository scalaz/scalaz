package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Unzip` */
trait UnzipV[F[_],A, B] extends SyntaxV[F[(A, B)]] {
  implicit def F: Unzip[F]
  ////
  def unfzip: (F[A], F[B]) =
    F.unzip(self)

  def firsts: F[A] =
    F.firsts(self)

  def seconds: F[B] =
    F.seconds(self)
  ////
}

trait ToUnzipV {
  implicit def ToUnzipV[F[_],A, B](v: F[(A, B)])(implicit F0: Unzip[F]) =
    new UnzipV[F,A, B] { def self = v; implicit def F: Unzip[F] = F0 }

  ////

  ////
}

trait UnzipSyntax[F[_]] {
  implicit def ToUnzipV[A, B](v: F[(A, B)])(implicit F0: Unzip[F]): UnzipV[F, A, B] = new UnzipV[F,A, B] { def self = v; implicit def F: Unzip[F] = F0 }

  ////

  ////
}
