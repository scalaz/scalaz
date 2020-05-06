package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Band` */
final class BandOps[F] private[syntax](val self: F)(implicit val F: Band[F]) extends Ops[F] {
  ////

  ////
}

trait ToBandOps extends ToSemigroupOps {
  implicit def ToBandOps[F](v: F)(implicit F0: Band[F]): BandOps[F] =
    new BandOps[F](v)

  ////

  ////
}

trait BandSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToBandOps(v: F): BandOps[F] = new BandOps[F](v)(BandSyntax.this.F)

  def F: Band[F]
  ////

  ////
}
