package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Order` */
trait OrderV[F] extends SyntaxV[F] {
  implicit def F: Order[F]
  ////
  def <(other: F): Boolean = F.lessThan(self, other)
  def <=(other: F): Boolean = F.lessThanOrEqual(self, other)
  def >(other: F): Boolean = F.greaterThan(self, other)
  def >=(other: F): Boolean = F.greaterThanOrEqual(self, other)
  def max(other: F): F = F.max(self, other)
  def min(other: F): F = F.min(self, other)
  def ?|?(other: F): Ordering = F.order(self, other)
  def lte(other: F): Boolean = F.lessThanOrEqual(self, other)
  def gte(other: F): Boolean = F.greaterThanOrEqual(self, other)
  def lt(other: F): Boolean = F.lessThan(self, other)
  def gt(other: F): Boolean = F.greaterThan(self, other)
  ////
}

trait ToOrderV extends ToEqualV {
  implicit def ToOrderV[F](v: F)(implicit F0: Order[F]) =
    new OrderV[F] { def self = v; implicit def F: Order[F] = F0 }

  ////

  ////
}

trait OrderSyntax[F] extends EqualSyntax[F] {
  implicit def ToOrderV(v: F)(implicit F0: Order[F]): OrderV[F] = new OrderV[F] { def self = v; implicit def F: Order[F] = F0 }

  ////

  ////
}
