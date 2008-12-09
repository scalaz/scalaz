// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

/**
 * A lazy tuple-7.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait LT7[+A, +B, +C, +D, +E, +F, +G] {
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

  /**
   * Part of the tuple.
   */
  def _7: G
}

/**
 * Functions over lazy tuple-7 values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT7 {
  /**
   * Construct a lazy tuple-7.
   */
  def lt7[A, B, C, D, E, F, G](a: => A, b: => B, c: => C, d: => D, e: => E, f: => F, g: => G): LT7[A, B, C, D, E, F, G] = new LT7[A, B, C, D, E, F, G] {
    def _1 = a
    def _2 = b
    def _3 = c
    def _4 = d
    def _5 = e
    def _6 = f
    def _7 = g    
  }

  /**
   * An extractor for lazy tuple-7 values that always matches and evaluate to scala tuples.
   */
  def unapply[A, B, C, D, E, F, G](t: LT7[A, B, C, D, E, F, G]): Option[(A, B, C, D, E, F, G)] = Some(t._1, t._2, t._3, t._4, t._5, t._6, t._7)

  /**
   * Evaluate the tuple-7 into a scala tuple.
   */
  implicit def toTuple[A, B, C, D, E, F, G](t: LT7[A, B, C, D, E, F, G]) = (t._1, t._2, t._3, t._4, t._5, t._6, t._7)
}
