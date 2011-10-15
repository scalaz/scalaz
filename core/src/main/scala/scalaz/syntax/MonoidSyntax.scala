package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
trait MonoidV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToMonoidSyntax extends ToSemigroupSyntax {
  implicit def ToMonoidV[F](v: F) =
    new MonoidV[F] { def self = v }

  ////
  def mzero[F](implicit F: Monoid[F]): F = F.zero
  ////
}

trait MonoidSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToMonoidV(v: F): MonoidV[F] = new MonoidV[F] { def self = v }

  ////
  def mzero(implicit F: Monoid[F]): F = F.zero
  ////
}
