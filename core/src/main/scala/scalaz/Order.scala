package scalaz

////
import scala.math.{Ordering => SOrdering}

/**
 * Safer version of [[scala.math.Ordering]].
 */
////
trait Order[F] extends Equal[F] { self =>
  ////
  def apply(x: F, y: F): Ordering = order(x, y)

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

  /** @note `Order.fromScalaOrdering(toScalaOrdering)` = `this` */
  def toScalaOrdering: SOrdering[F] = SOrdering.fromLessThan[F](lessThan)

  final def reverseOrder = new Order[F] {
    def order(x: F, y: F): Ordering = self.order(y, x)
  }

  trait OrderLaw extends EqualLaw {
    import std.boolean.conditional

    /** `order` yields a total order, in the mathematical sense. */
    def transitiveOrder(f1: F, f2: F, f3: F): Boolean = {
      val f1f2: Ordering = order(f1, f2)
      conditional(Set(f1f2, Ordering.EQ)(order(f2, f3)), order(f1, f3) == f1f2)
    }

    def orderAndEqualConsistent(f1: F, f2: F): Boolean = {
      conditional(equal(f1, f2), order(f1, f2) == Ordering.EQ)
    }
  }

  def orderLaw = new OrderLaw {}

  ////
  val orderSyntax = new scalaz.syntax.OrderSyntax[F] { def F = Order.this }
}

object Order {
  @inline def apply[F](implicit F: Order[F]): Order[F] = F

  ////

  implicit val orderInstance: Contravariant[Order] = new Contravariant[Order] {
    def contramap[A, B](r: Order[A])(f: (B) => A): Order[B] = r.contramap(f)
  }

  implicit def fromScalaOrdering[A](implicit O: SOrdering[A]): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = std.anyVal.intInstance.order(O.compare(a1, a2), 0)
  }

  /** Alias for `Order[B] contramap f`, with inferred `B`. */
  def orderBy[A, B: Order](f: A => B): Order[A] = Order[B] contramap f

  /** Derive from an `order` function. */
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
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
