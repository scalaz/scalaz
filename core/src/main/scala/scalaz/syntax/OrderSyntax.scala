package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Order` */
final class OrderOps[F] private[syntax](val self: F)(implicit val F: Order[F]) extends Ops[F] {
  ////
  final def <(other: F): Boolean = F.lessThan(self, other)
  final def <=(other: F): Boolean = F.lessThanOrEqual(self, other)
  final def >(other: F): Boolean = F.greaterThan(self, other)
  final def >=(other: F): Boolean = F.greaterThanOrEqual(self, other)
  final def max(other: F): F = F.max(self, other)
  final def min(other: F): F = F.min(self, other)
  final def cmp(other: F): Ordering = F.order(self, other)
  final def ?|?(other: F): Ordering = F.order(self, other)
  final def lte(other: F): Boolean = F.lessThanOrEqual(self, other)
  final def gte(other: F): Boolean = F.greaterThanOrEqual(self, other)
  final def lt(other: F): Boolean = F.lessThan(self, other)
  final def gt(other: F): Boolean = F.greaterThan(self, other)
  ////
}

trait ToOrderOps extends ToEqualOps {
  implicit def ToOrderOps[F](v: F)(implicit F0: Order[F]) =
    new OrderOps[F](v)

  ////

  ////
}

trait OrderSyntax[F] extends EqualSyntax[F] {
  implicit def ToOrderOps(v: F): OrderOps[F] = new OrderOps[F](v)(OrderSyntax.this.F)

  def F: Order[F]
  ////

  ////
}
