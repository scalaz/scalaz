// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

/**
 * A lazy tuple-3.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait LT3[+A, +B, +C] {
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
}

/**
 * Functions over lazy tuple-3 values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT3 {
  /**
   * Construct a lazy tuple-3.
   */
  def lt3[A, B, C](a: => A, b: => B, c: => C): LT3[A, B, C] = new LT3[A, B, C] {
    def _1 = a
    def _2 = b
    def _3 = c    
  }

  /**
   * An extractor for lazy tuple-3 values that always matches and evaluate to scala tuples.
   */
  def unapply[A, B, C](t: LT3[A, B, C]): Option[(A, B, C)] = Some(t._1, t._2, t._3)

  /**
   * Evaluate the tuple-3 into a scala tuple.
   */
  implicit def toTuple[A, B, C](t: LT3[A, B, C]) = (t._1, t._2, t._3)
}
