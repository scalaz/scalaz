package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Index` */
sealed abstract class IndexOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Index[F]
  ////
  @deprecated("Index#index is deprecated, use Foldable#index instead", "7.1")
  final def index(n: Int): Option[A] = F.index(self, n)
  @deprecated("Index#indexOr is deprecated, use Foldable#indexOr instead", "7.1")
  final def indexOr(default: => A, n: Int): A = F.indexOr(self, default, n)
  ////
}

trait ToIndexOps0 {
  implicit def ToIndexOpsUnapply[FA](v: FA)(implicit F0: Unapply[Index, FA]) =
    new IndexOps[F0.M,F0.A] { def self = F0(v); implicit def F: Index[F0.M] = F0.TC }

}

trait ToIndexOps extends ToIndexOps0 {
  implicit def ToIndexOps[F[_],A](v: F[A])(implicit F0: Index[F]) =
    new IndexOps[F,A] { def self = v; implicit def F: Index[F] = F0 }

  ////

  ////
}

trait IndexSyntax[F[_]]  {
  implicit def ToIndexOps[A](v: F[A]): IndexOps[F, A] = new IndexOps[F,A] { def self = v; implicit def F: Index[F] = IndexSyntax.this.F }

  def F: Index[F]
  ////

  ////
}
