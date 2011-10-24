package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Semigroup` */
trait SemigroupV[F] extends SyntaxV[F] {
  implicit def F: Semigroup[F]
  ////
  def |+|(other: => F): F = F.append(self, other)
  def mappend(other: => F): F = F.append(self, other)
  ////
}

trait ToSemigroupSyntax  {
  implicit def ToSemigroupV[F](v: F)(implicit F0: Semigroup[F]) =
    new SemigroupV[F] { def self = v; implicit def F: Semigroup[F] = F0 }

  ////
  ////
}

trait SemigroupSyntax[F]  {
  implicit def ToSemigroupV(v: F)(implicit F0: Semigroup[F]): SemigroupV[F] = new SemigroupV[F] { def self = v; implicit def F: Semigroup[F] = F0 }

  ////
  def mappend(f1: F, f2: => F)(implicit F: Semigroup[F]): F = F.append(f1, f2)

  ////
}
