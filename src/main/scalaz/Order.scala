// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Order between two values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Order[A] {
  /**
   * The ordering between the two given values.
   */
  def order(a1: A, a2: A): Ordering

  /**
   * Returns an order that, if equal tries the given order.
   */
  def ->>[B](f: A => B)(implicit o: Order[B]) = new Order[A] {
    def order(a1: A, a2: A) = {
      val t = Order.this.order(a1, a2)
      if(t == EQ) o.order(f(a1), f(a2))
      else t
    }
  }
}

import control.{Cofunctor, CofunctorW}
import control.CofunctorW.cofunctor

/**
 * Functions over order.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Order {
  /**
   * Constructs an <code>Order</code> from the given function.
   */
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }

  /**
   * Constructs an <code>Equal</code> from an <code>Order</code>.
   */
  def orderEqual[A](implicit o: Order[A]) = Equal.equal[A](o.order(_, _) == EQ)

  /**
   * Constructs an <code>Ordered</code> (less general) from an <code>Order</code>.
   */
  implicit def ordered[A](a: A)(implicit o: Order[A]): Ordered[A] = new Ordered[A] {
    def compare(aa: A) = o.order(a, aa).toInt
  }

  /**
   * An order for booleans.
   */
  implicit val OrderBoolean: Order[Boolean] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for bytes.
   */
  implicit val OrderByte: Order[Byte] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for characters.
   */
  implicit val OrderChar: Order[Char] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for doubles.
   */
  implicit val OrderDouble: Order[Double] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for floats.
   */
  implicit val OrderFloat: Order[Float] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for integers.
   */
  implicit val OrderInt: Order[Int] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for longs.
   */
  implicit val OrderLong: Order[Long] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for shorts.
   */
  implicit val OrderShort: Order[Short] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for strings.
   */
  implicit val OrderString: Order[String] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  /**
   * An order for the unit value.
   */
  implicit val OrderUnit: Order[Unit] = order((a1, a2) => EQ)

  /**
   * An order for the streams.
   */
  implicit def OrderStream[A](implicit o: Order[A]): Order[Stream[A]] = order[Stream[A]]((a1, a2) =>
    if(a1.isEmpty)
      if(a2.isEmpty) EQ
      else LT
    else if(a2.isEmpty)
      if(a1.isEmpty) EQ
      else GT
    else {
      val x = o.order(a1.head, a2.head)
      if(x == EQ)
        OrderStream(o).order(a1.tail, a2.tail)
      else
        x
    })

  /**
   * An order for the lists.
   */
  implicit def OrderList[A](implicit o: Order[A]): Order[List[A]] = order[List[A]] {
    case (Nil, Nil) => EQ
    case (Nil, _) => LT
    case (_, Nil) => GT
    case (a :: as, b :: bs) => {
      val x = o.order(a, b)
      if(x == EQ)
        OrderList(o).order(as, bs)
      else
        x
    }
  }

  /**
   * An order for the arrays.
   */
  implicit def OrderArray[A](implicit o: Order[A]): Order[Array[A]] = order[Array[A]]((a1, a2) => {
    val x = (a1 zip a2).map { case (x1, x2) => o.order(x1, x2) }.dropWhile(_ == EQ)
    if(x.isEmpty)
      OrderInt.order(a1.length, a2.length)
    else
      x(0)
  })

  /**
   * An order for the options.
   */
  implicit def OrderOption[A](implicit o: Order[A]): Order[Option[A]] = order[Option[A]] {
    case (Some(x), Some(y)) => o.order(x, y)
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  }

  /**
   * An order for the eithers.
   */
  implicit def OrderEither[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either[A, B]] {
    case (Left(x), Left(y)) => oa.order(x, y)
    case (Right(x), Right(y)) => ob.order(x, y)
    case (Left(_), Right(_)) => LT
    case (Right(_), Left(_)) => GT
  }

  /**
   * A contra-variant functor for <code>Order</code>.
   */
  implicit val OrderCofunctor = new Cofunctor[Order] {
    def comap[A, B](f: B => A, o: Order[A]) = new Order[B] {
      def order(b1: B, b2: B) = o.order(f(b1), f(b2))
    }
  }

  /**
   * A contra-variant functor wrapper for <code>Order</code>.
   */
  implicit def OrderCofunctorW[A](o: Order[A]): CofunctorW[Order, A] = cofunctor[Order](o)
}
