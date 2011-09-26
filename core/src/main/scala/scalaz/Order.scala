package scalaz

trait Order[F] extends Equal[F] { self =>
  ////
  // TODO
//  def order(a1: F, a2: F): Ordering
//
//  final def equal(a1: F, a2: F): Boolean = order(a1, a2) == EQ

  // derived functions

  ////
  val orderSyntax = new scalaz.syntax.OrderSyntax[F] {}
}

////
/**
 *
 */
////

object Order {
  def apply[F](implicit F: Order[F]): Order[F] = F

  ////

  ////
}

