package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
trait MonoidV[F] extends SyntaxV[F] {
  implicit def F: Monoid[F]
  ////

  ////
}

trait ToMonoidSyntax extends ToSemigroupSyntax {
  implicit def ToMonoidV[F](v: F)(implicit F0: Monoid[F]) =
    new MonoidV[F] { def self = v; implicit def F: Monoid[F] = F0 }

  ////
  def mzero[F](implicit F: Monoid[F]): F = F.zero
  ////
}

trait MonoidSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToMonoidV(v: F)(implicit F0: Monoid[F]): MonoidV[F] = new MonoidV[F] { def self = v; implicit def F: Monoid[F] = F0 }

  ////
  def mzero(implicit F: Monoid[F]): F = F.zero
  ////
}
