// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

/**
 * A lazy tuple-6.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait LT6[+A, +B, +C, +D, +E, +F] {
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

  /**
   * Part of the tuple.
   */
  def _6: F
}

/**
 * Functions over lazy tuple-6 values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT6 {
  /**
   * Construct a lazy tuple-6.
   */
  def lt6[A, B, C, D, E, F](a: => A, b: => B, c: => C, d: => D, e: => E, f: => F): LT6[A, B, C, D, E, F] = new LT6[A, B, C, D, E, F] {
    def _1 = a
    def _2 = b
    def _3 = c
    def _4 = d
    def _5 = e
    def _6 = f
  }

  /**
   * An extractor for lazy tuple-6 values that always matches and evaluate to scala tuples.
   */
  def unapply[A, B, C, D, E, F](t: LT6[A, B, C, D, E, F]): Option[(A, B, C, D, E, F)] = Some(t._1, t._2, t._3, t._4, t._5, t._6)

  /**
   * Evaluate the tuple-6 into a scala tuple.
   */
  implicit def toTuple[A, B, C, D, E, F](t: LT6[A, B, C, D, E, F]) = (t._1, t._2, t._3, t._4, t._5, t._6)
}
