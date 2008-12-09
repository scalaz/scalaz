// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

/**
 * A lazy tuple-2.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait LT2[+A, +B] {
  /**
   * Part of the tuple.
   */
  def _1: A

  /**
   * Part of the tuple.
   */
  def _2: B
}

/**
 * Functions over lazy tuple-2 values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT2 {
  /**
   * Construct a lazy tuple-2.
   */
  def lt2[A, B](a: => A, b: => B): LT2[A, B] = new LT2[A, B] {
    def _1 = a
    def _2 = b
  }

  /**
   * An extractor for lazy tuple-2 values that always matches and evaluate to scala tuples.
   */
  def unapply[A, B](t: LT2[A, B]): Option[(A, B)] = Some(t._1, t._2)

  /**
   * Evaluate the tuple-2 into a scala tuple.
   */
  implicit def toTuple[A, B](t: LT2[A, B]) = (t._1, t._2)
}