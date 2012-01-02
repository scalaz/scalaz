package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
trait MonoidV[F] extends SyntaxV[F] {
  implicit def F: Monoid[F]
  ////
  final def multiply(n: Int): F = F.multiply(self, n)

  ////
}

trait ToMonoidV extends ToSemigroupV {
  implicit def ToMonoidV[F](v: F)(implicit F0: Monoid[F]) =
    new MonoidV[F] { def self = v; implicit def F: Monoid[F] = F0 }

  ////
  implicit def ToMonoidV1[A](v: A) = new MonoidV1[A] { def self = v }
  trait MonoidV1[A] extends SyntaxV[A] {
    def replicate[F[_]](n: Int, f: A => A = (a: A) => a)(implicit P: Pointed[F], FA: Monoid[F[A]]): F[A] =
      Monoid.replicate[F, A](self)(n, f)

    trait Unfold[F[_]] {
      def apply[B](f: A => Option[(B, A)])(implicit F: Pointed[F], FB: Monoid[F[B]]): F[B] = Monoid.unfold[F, A, B](self)(f)
    }
    def unfold[F[_]] = new Unfold[F]{}
  }

  def mzero[F](implicit F: Monoid[F]): F = F.zero
  ////
}

trait MonoidSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToMonoidV(v: F)(implicit F0: Monoid[F]): MonoidV[F] = new MonoidV[F] { def self = v; implicit def F: Monoid[F] = F0 }

  ////
  def mzero(implicit F: Monoid[F]): F = F.zero
  ////
}
