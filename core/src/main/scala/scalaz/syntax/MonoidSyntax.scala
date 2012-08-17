package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
trait MonoidOps[F] extends Ops[F] {
  implicit def F: Monoid[F]
  ////
  final def multiply(n: Int): F = F.multiply(self, n)

  ////
}

trait ToMonoidOps extends ToSemigroupOps {
  implicit def ToMonoidOps[F](v: F)(implicit F0: Monoid[F]) =
    new MonoidOps[F] { def self = v; implicit def F: Monoid[F] = F0 }

  ////

  def mzero[F](implicit F: Monoid[F]): F = F.zero
  def ∅[F](implicit F: Monoid[F]): F = F.zero
  ////
}

trait MonoidSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToMonoidOps(v: F): MonoidOps[F] = new MonoidOps[F] { def self = v; implicit def F: Monoid[F] = MonoidSyntax.this.F }
  
  def F: Monoid[F]
  ////
  def mzero(implicit F: Monoid[F]): F = F.zero
  def ∅(implicit F: Monoid[F]): F = F.zero
  ////
}
