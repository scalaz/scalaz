package scalaz

////
import scala.math.{Ordering => SOrdering}

/**
 *
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

  def toScalaOrdering: SOrdering[F] = SOrdering.fromLessThan[F](lessThan)

  def reverse: Order[F] = {
    val self = this
    new Order[F] {
      def order(x: F, y: F) = self.order(y, x)

      override def reverse = self
    }
  }

  trait OrderLaw extends EqualLaw {
    import std.boolean.conditional

    def transitiveOrder(f1: F, f2: F, f3: F): Boolean = {
      val f1f2: Ordering = order(f1, f2)
      conditional(Set(f1f2, Ordering.EQ)(order(f2, f3)), order(f1, f3) == f1f2)
    }

    def orderAndEqualConsistent(f1: F, f2: F): Boolean = {
      conditional(equal(f1, f2), order(f1, f2) == Ordering.EQ)
    }
    
    def reverseConsistentWithFlip(f1: F, f2: F): Boolean = {
      order(f1, f2) == reverse(f2, f1)
    }
    
    def doubleReverseIsIdentity(f1: F, f2: F): Boolean = {
      order(f1, f2) == reverse.reverse(f1, f2)
    }
  }

  def orderLaw = new OrderLaw {}

  ////
  val orderSyntax = new scalaz.syntax.OrderSyntax[F] {}
}

object Order {
  @inline def apply[F](implicit F: Order[F]): Order[F] = F

  ////
  implicit val orderInstance: Contravariant[Order] = new Contravariant[Order] {
    def contramap[A, B](r: Order[A])(f: (B) => A): Order[B] = r.contramap(f)
  }

  def orderBy[A, B: Order](f: A => B): Order[A] = Order[B] contramap f

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

