// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

/**
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait LT5[+A, +B, +C, +D, +E] {
  /**
   * Part of the tuple.
   */
  def _1: A

  /**
   * Part of the tuple.
   */
  def _2: B

  /**
   * Part of the tuple.
   */
  def _3: C

  /**
   * Part of the tuple.
   */
  def _4: D

  /**
   * Part of the tuple.
   */
  def _5: E
}

/**
 * Functions over lazy tuple-5 values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT5 {
  /**
   * Construct a lazy tuple-5.
   */
  def lt5[A, B, C, D, E](a: => A, b: => B, c: => C, d: => D, e: => E): LT5[A, B, C, D, E] = new LT5[A, B, C, D, E] {
    def _1 = a
    def _2 = b
    def _3 = c
    def _4 = d
    def _5 = e
  }

  /**
   * An extractor for lazy tuple-5 values that always matches and evaluate to scala tuples.
   */
  def unapply[A, B, C, D, E](t: LT5[A, B, C, D, E]): Option[(A, B, C, D, E)] = Some(t._1, t._2, t._3, t._4, t._5)

  /**
   * Evaluate the tuple-5 into a scala tuple.
   */
  implicit def toTuple[A, B, C, D, E](t: LT5[A, B, C, D, E]) = (t._1, t._2, t._3, t._4, t._5)
}
