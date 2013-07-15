package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Semigroup` */
sealed abstract class SemigroupOps[F] extends Ops[F] {
  implicit def F: Semigroup[F]
  ////
  final def |+|(other: => F): F = F.append(self, other)
  final def mappend(other: => F): F = F.append(self, other)
  final def ⊹(other: => F): F = F.append(self, other)
  ////
}

trait ToSemigroupOps  {
  implicit def ToSemigroupOps[F](v: F)(implicit F0: Semigroup[F]) =
    new SemigroupOps[F] { def self = v; implicit def F: Semigroup[F] = F0 }

  ////
  ////
}

trait SemigroupSyntax[F]  {
  implicit def ToSemigroupOps(v: F): SemigroupOps[F] = new SemigroupOps[F] { def self = v; implicit def F: Semigroup[F] = SemigroupSyntax.this.F }
  
  def F: Semigroup[F]
  ////
  def mappend(f1: F, f2: => F)(implicit F: Semigroup[F]): F = F.append(f1, f2)

  ////
}
