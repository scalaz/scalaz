package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Nondeterminism` */
sealed abstract class NondeterminismOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Nondeterminism[F]
  ////

  ////
}

trait ToNondeterminismOps0 {
  implicit def ToNondeterminismOpsUnapply[FA](v: FA)(implicit F0: Unapply[Nondeterminism, FA]) =
    new NondeterminismOps[F0.M,F0.A] { def self = F0(v); implicit def F: Nondeterminism[F0.M] = F0.TC }

}

trait ToNondeterminismOps extends ToNondeterminismOps0 with ToMonadOps {
  implicit def ToNondeterminismOps[F[_],A](v: F[A])(implicit F0: Nondeterminism[F]) =
    new NondeterminismOps[F,A] { def self = v; implicit def F: Nondeterminism[F] = F0 }

  ////

  ////
}

trait NondeterminismSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToNondeterminismOps[A](v: F[A]): NondeterminismOps[F, A] = new NondeterminismOps[F,A] { def self = v; implicit def F: Nondeterminism[F] = NondeterminismSyntax.this.F }

  def F: Nondeterminism[F]
  ////

  ////
}
