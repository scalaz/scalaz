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

  def sort(x: F, y: F) = if (lessThanOrEqual(x, y)) (x, y) else (y, x)

  override def contramap[B](f: B => F): Order[B] = new Order[B] {
    def order(b1: B, b2: B): Ordering = self.order(f(b1), f(b2))
    override def equal(b1: B, b2: B) = self.equal(f(b1), f(b2))
  }

  /** @note `Order.fromScalaOrdering(toScalaOrdering).order(x, y)`
            = `this.order(x, y)` */
  def toScalaOrdering: SOrdering[F] = new SOrdering[F] {
    def compare(x: F, y: F) = self.order(x, y).toInt
  }

  def reverseOrder: Order[F] = new Order[F] {
    def order(x: F, y: F): Ordering = self.order(y, x)
    override def equal(x: F, y: F) = self.equal(x, y)
    override def equalIsNatural = self.equalIsNatural
    override def reverseOrder = self
  }

  trait OrderLaw extends EqualLaw {
    import std.boolean.conditional

    /** f1 < f2 means f2 > f1, and so on. */
    def antisymmetric(f1: F, f2: F): Boolean =
      order(f1, f2).complement == order(f2, f1)

    /** `order` yields a total order, in the mathematical sense. */
    def transitiveOrder(f1: F, f2: F, f3: F): Boolean = {
      val f1f2: Ordering = order(f1, f2)
      conditional(Set(f1f2, Ordering.EQ)(order(f2, f3)), order(f1, f3) == f1f2)
    }

    def orderAndEqualConsistent(f1: F, f2: F): Boolean = {
      equal(f1, f2) == (order(f1, f2) == Ordering.EQ)
    }
  }

  def orderLaw = new OrderLaw {}

  ////
  val orderSyntax = new scalaz.syntax.OrderSyntax[F] { def F = Order.this }
}

object Order {
  @inline def apply[F](implicit F: Order[F]): Order[F] = F

  ////

  implicit val orderInstance: Divisible[Order] = new Divisible[Order] {
    def contramap[A, B](r: Order[A])(f: B => A) = r.contramap(f)

    override def conquer[A] = order((_, _) => Ordering.EQ)

    override def divide[A, B, C](fa: Order[A], fb: Order[B])(f: C => (A, B)) =
      order[C]{ (c1, c2) =>
        val (a1, b1) = f(c1)
        val (a2, b2) = f(c2)
        fa.order(a1, a2) match {
          case Ordering.EQ => fb.order(b1, b2)
          case o => o
        }
      }
  }

  def fromScalaOrdering[A](implicit O: SOrdering[A]): Order[A] = new Order[A] {
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
