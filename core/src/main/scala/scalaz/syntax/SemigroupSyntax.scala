package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Semigroup` */
trait SemigroupV[F] extends SyntaxV[F] {
  ////
  def |+|(other: => F)(implicit F: Semigroup[F]): F = F.append(self, other)
  def mappend(other: => F)(implicit F: Semigroup[F]): F = F.append(self, other)
  ////
}

trait ToSemigroupSyntax  {
  implicit def ToSemigroupV[F](v: F) =
    new SemigroupV[F] { def self = v }

  ////
  ////
}

trait SemigroupSyntax[F]  {
  implicit def ToSemigroupV(v: F): SemigroupV[F] = new SemigroupV[F] { def self = v }

  ////
  def mappend(f1: F, f2: => F)(implicit F: Semigroup[F]): F = F.append(f1, f2)

  ////
}
