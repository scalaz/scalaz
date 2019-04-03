package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Semigroup` */
final class SemigroupOps[F] private[syntax](val self: F)(implicit val F: Semigroup[F]) extends Ops[F] {
  ////
  final def |+|(other: => F): F = F.append(self, other)
  final def mappend(other: => F): F = F.append(self, other)
  final def ⊹(other: => F): F = F.append(self, other)
  ////
}

trait ToSemigroupOps  {
  implicit def ToSemigroupOps[F](v: F)(implicit F0: Semigroup[F]) =
    new SemigroupOps[F](v)

  ////
  ////
}

trait SemigroupSyntax[F]  {
  implicit def ToSemigroupOps(v: F): SemigroupOps[F] = new SemigroupOps[F](v)(SemigroupSyntax.this.F)

  def F: Semigroup[F]
  ////
  def mappend(f1: F, f2: => F)(implicit F: Semigroup[F]): F = F.append(f1, f2)

  ////
}
