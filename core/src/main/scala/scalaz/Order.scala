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
  def lessThan(x: F, y: F): Boolean = order(x, y) == Ordering.LT

  def lessThanOrEqual(x: F, y: F): Boolean = order(x, y) != Ordering.GT

  def greaterThan(x: F, y: F): Boolean = order(x, y) == Ordering.GT

  def greaterThanOrEqual(x: F, y: F): Boolean = order(x, y) != Ordering.LT

  def max(x: F, y: F): F = if (greaterThanOrEqual(x, y)) x else y

  def min(x: F, y: F): F = if (lessThan(x, y)) x else y

  def sort(x: F, y: F): (F, F) = if (lessThanOrEqual(x, y)) (x, y) else (y, x)

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

  import Isomorphism._

  def fromIso[F, G](D: F <=> G)(implicit M: Order[G]): Order[F] =
    new IsomorphismOrder[F, G] {
      override def G: Order[G] = M
      override def iso: F <=> G = D
    }

  ////

  implicit val orderInstance: Decidable[Order] = new OrderDecidableInstance

  private[scalaz] class OrderDecidableInstance extends Decidable[Order] with Divisible[Order] {
    override def contramap[A, B](r: Order[A])(f: B => A): Order[B] = r.contramap(f)

    override def choose2[Z, A1, A2](order1: => Order[A1], order2: => Order[A2])(f: Z => A1 \/ A2): Order[Z] = order[Z] { (c1, c2) =>
      f(c1) match {
        case -\/(c) => f(c2) match {
          case -\/(d) => order1(c, d)
          case _ => Ordering.LT
        }
        case \/-(c) => f(c2) match {
          case \/-(d) => order2(c, d)
          case _ => Ordering.GT
        }
      }
    }

    override def conquer[A]: Order[A] = order((_, _) => Ordering.EQ)

    override def divide2[A1, A2, Z](a1: => Order[A1], a2: => Order[A2])(f: Z => (A1, A2)): Order[Z] = order[Z] { (c1, c2) =>
      val (x1, y1) = f(c1)
      val (x2, y2) = f(c2)
      a1.order(x1, x2) match {
        case Ordering.EQ => a2.order(y1, y2)
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

  implicit def orderMonoid[A]: Monoid[Order[A]] = new Monoid[Order[A]] {
    def zero: Order[A] = new Order[A] {
      def order(x: A, y: A): Ordering = Monoid[Ordering].zero
    }
    def append(f1: Order[A], f2: => Order[A]): Order[A] = new Order[A] {
      def order(x: A, y: A): Ordering = Semigroup[Ordering].append(f1.order(x, y), f2.order(x, y))
    }
  }

  ////
}

trait IsomorphismOrder[F, G] extends Order[F] with IsomorphismEqual[F, G]{
  implicit def G: Order[G]
  ////
  override def equal(x: F, y: F): Boolean =
    super[IsomorphismEqual].equal(x, y)

  override def order(x: F, y: F): Ordering =
    G.order(iso.to(x), iso.to(y))
  ////
}
