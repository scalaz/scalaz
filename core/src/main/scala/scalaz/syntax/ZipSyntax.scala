package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Zip` */
final class ZipOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Zip[F]) extends Ops[F[A]] {
  ////
  final def fzip[B](b: => F[B]): F[(A, B)] = F.zip(self, b)
  final def fzipWith[B, C](b: => F[B])(f: (A, B) => C)(implicit T: Functor[F]): F[C] = F.zipWith(self, b)(f)
  final def apzip[B](b: => F[A] => F[B]): F[(A, B)] = F.apzip(b, self)
  // alias for apzip
  final def <*|*>[B](b: => F[A] => F[B]): F[(A, B)] = F.apzip(b, self)
  ////
}

sealed trait ToZipOps0 {
  implicit def ToZipOpsUnapply[FA](v: FA)(implicit F0: Unapply[Zip, FA]) =
    new ZipOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToZipOps extends ToZipOps0 {
  implicit def ToZipOps[F[_],A](v: F[A])(implicit F0: Zip[F]) =
    new ZipOps[F,A](v)

  ////

  ////
}

trait ZipSyntax[F[_]]  {
  implicit def ToZipOps[A](v: F[A]): ZipOps[F, A] = new ZipOps[F,A](v)(ZipSyntax.this.F)

  def F: Zip[F]
  ////

  ////
}
