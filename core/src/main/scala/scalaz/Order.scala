package scalaz

trait Order[F] extends Equal[F] { self =>
  ////
  def order(x: F, y: F): Ordering 
  
  final def equal(x: F, y: F): Boolean = order(x, y) == Ordering.EQ

  // derived functions
  def lessThan(x: F, y: F) = order(x, y) == Ordering.LT

  def lessThanOrEqual(x: F, y: F) = order(x, y) != Ordering.GT

  def greaterThan(x: F, y: F) = order(x, y) == Ordering.GT

  def greaterThanOrEqual(x: F, y: F) = order(x, y) != Ordering.LT

  def max(x: F, y: F) = if (greaterThanOrEqual(x, y)) x else y

  def min(x: F, y: F) = if (lessThan(x, y)) x else y

  def contramap[B](f: B => F): Order[B] = new Order[B] {
    def order(b1: B, b2: B): Ordering = self.order(f(b1), f(b2))
  }

  ////
  val orderSyntax = new scalaz.syntax.OrderSyntax[F] {}
}

object Order {
  def apply[F](implicit F: Order[F]): Order[F] = F

  ////
  implicit object order extends Contravariant[Order] {
    def contramap[A, B](r: Order[A], f: (B) => A): Order[B] = r.contramap(f)
  }

  ////
}

