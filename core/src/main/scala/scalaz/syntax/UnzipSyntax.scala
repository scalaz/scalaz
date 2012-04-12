package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Unzip` */
trait UnzipV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Unzip[F]
  ////
  def unfzip[B](a: F[(A, B)]): (F[A], F[B]) =
    F.unzip(a)

  def firsts[B](a: F[(A, B)]): F[A] =
    F.firsts(a)

  def seconds[B](a: F[(A, B)]): F[B] =
    F.seconds(a)
  ////
}

trait ToUnzipV {
  implicit def ToUnzipV[F[_],A](v: F[A])(implicit F0: Unzip[F]) =
    new UnzipV[F,A] { def self = v; implicit def F: Unzip[F] = F0 }

  ////

  ////
}

trait UnzipSyntax[F[_]] {
  implicit def ToUnzipV[A](v: F[A])(implicit F0: Unzip[F]): UnzipV[F, A] = new UnzipV[F,A] { def self = v; implicit def F: Unzip[F] = F0 }

  ////

  ////
}
