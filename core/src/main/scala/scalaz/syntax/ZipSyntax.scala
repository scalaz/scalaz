package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Zip` */
sealed abstract class ZipOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Zip[F]
  ////
  final def fzip[B](b: => F[B]): F[(A, B)] = F.zip(self, b)
  final def fzipWith[B, C](b: => F[B])(f: (A, B) => C)(implicit T: Functor[F]): F[C] = F.zipWith(self, b)(f)
  final def apzip[B](b: => F[A] => F[B]): F[(A, B)] = F.apzip(b, self)
  // alias for apzip
  final def <*|*>[B](b: => F[A] => F[B]): F[(A, B)] = F.apzip(b, self)
  ////
}

trait ToZipOps0 {
  implicit def ToZipOpsUnapply[FA](v: FA)(implicit F0: Unapply[Zip, FA]) =
    new ZipOps[F0.M,F0.A] { def self = F0(v); implicit def F: Zip[F0.M] = F0.TC }

}

trait ToZipOps extends ToZipOps0 {
  implicit def ToZipOps[F[_],A](v: F[A])(implicit F0: Zip[F]) =
    new ZipOps[F,A] { def self = v; implicit def F: Zip[F] = F0 }

  ////

  ////
}

trait ZipSyntax[F[_]]  {
  implicit def ToZipOps[A](v: F[A]): ZipOps[F, A] = new ZipOps[F,A] { def self = v; implicit def F: Zip[F] = ZipSyntax.this.F }

  def F: Zip[F]
  ////

  ////
}
