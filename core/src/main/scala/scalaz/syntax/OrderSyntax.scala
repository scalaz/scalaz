package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Order` */
trait OrderV[F] extends SyntaxV[F] {
  implicit def F: Order[F]
  ////

  ////
}

trait ToOrderSyntax extends ToEqualSyntax {
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
