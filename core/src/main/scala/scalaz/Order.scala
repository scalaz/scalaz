package scalaz

////
/**
 *
 */
////
trait Order[F] extends Equal[F] { self =>
  ////
  def order(x: F, y: F): Ordering 
  
  def equal(x: F, y: F): Boolean = order(x, y) == Ordering.EQ

  // derived functions
  def lessThan(x: F, y: F) = order(x, y) == Ordering.LT

  def lessThanOrEqual(x: F, y: F) = order(x, y) != Ordering.GT

  def greaterThan(x: F, y: F) = order(x, y) == Ordering.GT

  def greaterThanOrEqual(x: F, y: F) = order(x, y) != Ordering.LT

  def max(x: F, y: F) = if (greaterThanOrEqual(x, y)) x else y

  def min(x: F, y: F) = if (lessThan(x, y)) x else y

  override def contramap[B](f: B => F): Order[B] = new Order[B] {
    def order(b1: B, b2: B): Ordering = self.order(f(b1), f(b2))
  }

  ////
  val orderSyntax = new scalaz.syntax.OrderSyntax[F] {}
}

object Order {
  @inline def apply[F](implicit F: Order[F]): Order[F] = F

  ////
  implicit val orderInstance: Contravariant[Order] = new Contravariant[Order] {
    def contramap[A, B](r: Order[A])(f: (B) => A): Order[B] = r.contramap(f)
  }

  implicit def orderMonoid[A] = new Monoid[Order[A]] {
    def zero: Order[A] = new Order[A] {
      def order(x: A, y: A): Ordering = Monoid[Ordering].zero
    }
    def append(f1: Order[A], f2: => Order[A]): Order[A] = new Order[A] {
      def order(x: A, y: A): Ordering = Semigroup[Ordering].append(f1.order(x, y), f2.order(x, y))
    }
  }

  ////
}

