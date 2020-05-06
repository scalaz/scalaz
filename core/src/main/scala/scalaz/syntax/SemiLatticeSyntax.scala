package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `SemiLattice` */
final class SemiLatticeOps[F] private[syntax](val self: F)(implicit val F: SemiLattice[F]) extends Ops[F] {
  ////

  ////
}

trait ToSemiLatticeOps extends ToBandOps {
  implicit def ToSemiLatticeOps[F](v: F)(implicit F0: SemiLattice[F]): SemiLatticeOps[F] =
    new SemiLatticeOps[F](v)

  ////

  ////
}

trait SemiLatticeSyntax[F] extends BandSyntax[F] {
  implicit def ToSemiLatticeOps(v: F): SemiLatticeOps[F] = new SemiLatticeOps[F](v)(SemiLatticeSyntax.this.F)

  def F: SemiLattice[F]
  ////

  ////
}
