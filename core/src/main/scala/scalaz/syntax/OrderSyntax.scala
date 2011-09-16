package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Order` */
trait OrderV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToOrderSyntax extends ToEqualSyntax {
  implicit def order[F](v: F) =
    (new OrderSyntax[F] {}).orderV(v)
}

trait OrderSyntax[F] extends EqualSyntax[F] {
  implicit def orderV(v: F): OrderV[F] = new OrderV[F] { def self = v }

  ////

  ////
}
