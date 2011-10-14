package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Order` */
trait OrderV[F] extends SyntaxV[F] {
  ////

  ////
}

trait ToOrderSyntax extends ToEqualSyntax {
  implicit def ToOrderV[F](v: F) =
    new OrderV[F] { def self = v }

  ////

  ////
}

trait OrderSyntax[F] extends EqualSyntax[F] {
  implicit def ToOrderV(v: F): OrderV[F] = new OrderV[F] { def self = v }

  ////

  ////
}
