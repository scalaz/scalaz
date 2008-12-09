// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Wraps an argument of <code>Equal</code> implementations.
 *
 * @see Equal
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait EqualW[A] {
  /**
   * Returns <code>true</code> if this is equal to the given argument, otherwise <code>false</code>.
   */
  def ===(a: A): Boolean

  /**
   * Returns <code>false</code> if this is equal to the given argument, otherwise <code>true</code>.
   */
  def /=(a: A) = !(===(a))
}

/**
 * Functions over equal wrapper values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object EqualW {
  /**
   * Constructs an <code>EqualW</code> from the given value and <code>Equal</code> implementation.
   */
  implicit def equal[A](a: A)(implicit e: Equal[A]): EqualW[A] = new EqualW[A] {
    def ===(aa: A) = e.equal(a, aa)
  }
}
