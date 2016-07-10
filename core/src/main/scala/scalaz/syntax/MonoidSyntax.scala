package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoid` */
final class MonoidOps[F] private[syntax](val self: F)(implicit val F: Monoid[F]) extends Ops[F] {
  ////
  final def multiply(n: Int): F = F.multiply(self, n)

  final def ifEmpty[A](tv: => A)(fv: => A)(implicit e: Equal[F]): A = F.ifEmpty(self)(tv)(fv)

  final def isMZero(implicit e: Equal[F]): Boolean = F.isMZero(self)

  final def onNotEmpty[A](v: => A)(implicit ma: Monoid[A], e: Equal[F]): A = F.onNotEmpty(self)(v)

  final def onEmpty[A](v: => A)(implicit ma: Monoid[A], e: Equal[F]): A = F.onEmpty(self)(v)
  ////
}

trait ToMonoidOps extends ToSemigroupOps {
  implicit def ToMonoidOps[F](v: F)(implicit F0: Monoid[F]) =
    new MonoidOps[F](v)

  ////

  def mzero[F](implicit F: Monoid[F]): F = F.zero
  def ∅[F](implicit F: Monoid[F]): F = F.zero
  ////
}

trait MonoidSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToMonoidOps(v: F): MonoidOps[F] = new MonoidOps[F](v)(MonoidSyntax.this.F)

  def F: Monoid[F]
  ////
  def mzero(implicit F: Monoid[F]): F = F.zero
  def ∅(implicit F: Monoid[F]): F = F.zero
  ////
}
