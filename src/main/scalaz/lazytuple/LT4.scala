// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

/**
 * A lazy tuple-4.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait LT4[+A, +B, +C, +D] {
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
}

/**
 * Functions over lazy tuple-4 values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT4 {
  /**
   * Construct a lazy tuple-4.
   */
  def lt4[A, B, C, D](a: => A, b: => B, c: => C, d: => D): LT4[A, B, C, D] = new LT4[A, B, C, D] {
    def _1 = a
    def _2 = b
    def _3 = c
    def _4 = d    
  }

  /**
   * An extractor for lazy tuple-4 values that always matches and evaluate to scala tuples.
   */
  def unapply[A, B, C, D](t: LT4[A, B, C, D]): Option[(A, B, C, D)] = Some(t._1, t._2, t._3, t._4)

  /**
   * Evaluate the tuple-4 into a scala tuple.
   */
  implicit def toTuple[A, B, C, D](t: LT4[A, B, C, D]) = (t._1, t._2, t._3, t._4)
}
