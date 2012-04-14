package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Zip` */
trait ZipV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Zip[F]
  ////
  final def fzip[B](b: => F[B]): F[(A, B)] = F.zip(self, b)
  final def fzipWith[B, C](b: => F[B])(f: (A, B) => C)(implicit T: Functor[F]): F[C] = F.zipWith(self, b)(f)
  final def apzip[B](b: => F[A] => F[B]): F[(A, B)] = F.apzip(b, self)
  // alias for apzip
  final def <*|*>[B](b: => F[A] => F[B]): F[(A, B)] = F.apzip(b, self)
  ////
}

trait ToZipV {
  implicit def ToZipV[F[_],A](v: F[A])(implicit F0: Zip[F]) =
    new ZipV[F,A] { def self = v; implicit def F: Zip[F] = F0 }

  ////

  ////
}

trait ZipSyntax[F[_]] {
  implicit def ToZipV[A](v: F[A])(implicit F0: Zip[F]): ZipV[F, A] = new ZipV[F, A] { def self = v; implicit def F: Zip[F] = F0 }

  ////

  ////
}
