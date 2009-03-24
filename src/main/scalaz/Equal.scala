// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Equality between two values that serves as a suitable replacement for (the defective)
 * <code>java.lang.Object.equals</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Equal[A] {
 /**
  * Compares the two given values for equality.
  */
  def equal(a1: A, a2: A): Boolean

  /**
   * Returns an equal that, if <code>true</code> tries the given equal.
   */
  def ->>[B](f: A => B)(implicit e: Equal[B]) = new Equal[A] {
    def equal(a1: A, a2: A) =
      Equal.this.equal(a1, a2) && e.equal(f(a1), f(a2))
  }
}

import list.NonEmptyList
import validation.Validation
import control.{Cofunctor, CofunctorW}
import control.CofunctorW.cofunctor

/**
 * Functions over equality.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Equal {
  /**
   * Constructs an <code>Equal</code> from the given function.
   */
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  /**
   * Constructs an <code>Equal</code> that uses <code>java.lang.Object.equals</code>.
   */
  def EqualA[A] = equal[A](_ == _)

  /**
   * Equality for booleans.
   */
  implicit val EqualBoolean = EqualA[Boolean]

  /**
   * Equality for bytes.
   */
  implicit val EqualByte = EqualA[Byte]

  /**
   * Equality for characters.
   */
  implicit val EqualChar = EqualA[Char]

  /**
   * Equality for doubles.
   */
  implicit val EqualDouble = EqualA[Double]

  /**
   * Equality for floats.
   */
  implicit val EqualFloat = EqualA[Float]

  /**
   * Equality for integers.
   */
  implicit val EqualInt = EqualA[Int]

  /**
   * Equality for longs.
   */
  implicit val EqualLong = EqualA[Long]

  /**
   * Equality for shorts.
   */
  implicit val EqualShort = EqualA[Short]

  /**
   * Equality for strings.
   */
  implicit val EqualString = EqualA[String]

  /**
   * Equality for unit values.
   */
  implicit val EqualUnit = EqualA[Unit]

  /**
   * Equality for streams.
   */
  implicit def EqualStream[A](implicit e: Equal[A]): Equal[Stream[A]] = equal[Stream[A]]((a1, a2) => {
    var x1 = a1
    var x2 = a2
    var b = false

    while(!x1.isEmpty && !x2.isEmpty && !b) {
      if(!e.equal(x1.head, x2.head))
        b = true
      else {
        x1 = x1.tail
        x2 = x2.tail
      }
    }

    x1.isEmpty && x2.isEmpty        
  })

  /**
   * Equality for lists.
   */
  implicit def EqualList[A](implicit e: Equal[A]): Equal[List[A]] = equal[List[A]]((a1, a2) => {
    var x1 = a1
    var x2 = a2
    var b = false

    while(!x1.isEmpty && !x2.isEmpty && !b) {
      if(!e.equal(x1.head, x2.head))
        b = true
      else {
        x1 = x1.tail
        x2 = x2.tail
      }
    }

    x1.isEmpty && x2.isEmpty    
  })

  /**
   * Equality for arrays.
   */
  implicit def EqualArray[A](implicit e: Equal[A]) = equal[Array[A]]((a1, a2) =>
          a1.length == a2.length && a1.zip(a2).forall { case (a, b) => e.equal(a, b) })

  /**
   * Equality for options.
   */
  implicit def EqualOption[A](implicit e: Equal[A]) = equal[Option[A]] {
    case (Some(x), Some(y)) => e.equal(x, y)
    case (a1, a2) => a1.isDefined == a2.isDefined
  }

  /**
   * Equality for non-empty lists.
   */
  implicit def EqualNonEmptyList[A](implicit e: Equal[A]): Equal[NonEmptyList[A]] =
    equal[NonEmptyList[A]](EqualList(e).equal(_, _))

  /**
   * Equality for either values.
   */
  implicit def EqualEither[A, B](implicit ea: Equal[A], eb: Equal[B]) = equal[Either[A, B]] {
    case (Left(a1), Left(a2)) => ea.equal(a1, a2)
    case (Right(a1), Right(a2)) => eb.equal(a1, a2)
    case _ => false
  }

  /**
   * Equality for validation values.
   */
  implicit def EqualValidation[A, B](implicit ea: Equal[A], eb: Equal[B]): Equal[Validation[A, B]] = EqualEither[A, B] < (_.either)

  /**
   * A contra-variant functor for <code>Equal</code>.
   */
  implicit val EqualCofunctor = new Cofunctor[Equal] {
    def comap[A, B](f: B => A, e: Equal[A]) = new Equal[B] {
      def equal(b1: B, b2: B) = e.equal(f(b1), f(b2))
    }
  }

  /**
   * A contra-variant functor wrapper for <code>Equal</code>. 
   */
  implicit def EqualCofunctorW[A](e: Equal[A]): CofunctorW[Equal, A] = cofunctor[Equal](e)

  /**
   * Equality for tuple-1.
   */
  implicit def EqualP1[A](implicit ea: Equal[A]) = equal[Tuple1[A]] { case (a1, a2) => ea.equal(a1._1, a2._1) }

  /**
   * Equality for tuple-2.
   */
  implicit def EqualP2[A, B](implicit ea: Equal[A], eb: Equal[B]) = equal[Tuple2[A, B]] { case ((a1, b1), (a2, b2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) }

  /**
   * Equality for tuple-3.
   */
  implicit def EqualP3[A, B, C](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C]) = equal[Tuple3[A, B, C]] { case ((a1, b1, c1), (a2, b2, c2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) &&
    ec.equal (c1, c2)}

  /**
   * Equality for tuple-4.
   */
  implicit def EqualP4[A, B, C, D](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D]) = equal[Tuple4[A, B, C, D]] { case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) &&
    ec.equal(c1, c2) &&
    ed.equal(d1, d2)}

  /**
   * Equality for tuple-5.
   */
  implicit def EqualP5[A, B, C, D, E](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E]) = equal[Tuple5[A, B, C, D, E]] { case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) &&
    ec.equal(c1, c2) &&
    ed.equal(d1, d2) &&
    ee.equal(e1, e2)}

  /**
   * Equality for tuple-6.
   */
  implicit def EqualP6[A, B, C, D, E, F](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F]) = equal[Tuple6[A, B, C, D, E, F]] { case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) &&
    ec.equal(c1, c2) &&
    ed.equal(d1, d2) &&
    ee.equal(e1, e2) &&
    ef.equal(f1, f2)}

  /**
   * Equality for tuple-7.
   */
  implicit def EqualP7[A, B, C, D, E, F, G](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F], eg: Equal[G]) = equal[Tuple7[A, B, C, D, E, F, G]] { case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) &&
    ec.equal(c1, c2) &&
    ed.equal(d1, d2) &&
    ee.equal(e1, e2) &&
    ef.equal(f1, f2) &&
    eg.equal(g1, g2)}

  /**
   * Equality for tuple-8.
   */
  implicit def EqualP8[A, B, C, D, E, F, G, H](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F], eg: Equal[G], eh: Equal[H]) = equal[Tuple8[A, B, C, D, E, F, G, H]] { case ((a1, b1, c1, d1, e1, f1, g1, h1), (a2, b2, c2, d2, e2, f2, g2, h2)) =>
    ea.equal(a1, a2) &&
    eb.equal(b1, b2) &&
    ec.equal(c1, c2) &&
    ed.equal(d1, d2) &&
    ee.equal(e1, e2) &&
    ef.equal(f1, f2) &&
    eg.equal(g1, g2) &&
    eh.equal(h1, h2)}
}
