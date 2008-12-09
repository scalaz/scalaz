// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.function

/**
 * Wraps <code>scala.Function2</code> and provides additional methods.
 *
 * @see scala.Function2
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Function2[-T1, -T2, +R] {
  /**
   * Apply the given arguments.
   */
  def apply(v1: T1, v2: T2): R

 /**
  * Flips the order of the arguments of this function.
  */
  def flip = (v2: T2, v1: T1) => apply(v1, v2)

  /**
   * Curries the arguments of this function.
   */
  def curry = (v1: T1) => ((v2: T2) => apply(v1, v2))

  /**
   * Flips, then curries the order of the arguments of this function.
   */
  def flipCurry = (v2: T2) => ((v1: T1) => apply(v1, v2))
}

/**
 * Functions over function-2.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Function2 {
  /**
   * Wraps a <code>scala.Function2</code>.
   */
  implicit def ScalaFunction2Function1[T1, T2, R](f: (T1, T2) => R): Function2[T1, T2, R] = new Function2[T1, T2, R] {
    def apply(v1: T1, v2: T2) = f(v1, v2)
  }

  /**
   * Unwraps a <code>scala.Function2</code>.
   */
  implicit def Function1ScalaFunction2[T1, T2, R](f: Function2[T1, T2, R]) = (v1: T1, v2: T2) => f(v1, v2)
}
