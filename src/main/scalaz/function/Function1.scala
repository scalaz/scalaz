// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.function


/**
 * Wraps <code>scala.Function1</code> and provides additional methods.
 *
 * @see scala.Function1
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Function1[-T, +R] {
  /**
   * Apply the given arguments.
   */
  def apply(t :T): R

  /**
   * Map the given function.
   */
  def map[B](g: R => B) = g compose (apply(_ :T))

  /**
   * Bind the given function.
   */
  def flatMap[B, TT <: T](g: R => Function1[TT, B]) =
    (t: TT) => g(Function1.this(t))(t)
}

/**
 * Functions over function-1.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Function1 {
  /**
   * Wraps a <code>scala.Function1</code>.
   */
  implicit def ScalaFunction1Function1[T, R](f: T => R) = new Function1[T, R] {
    def apply(t: T) = f(t)
  }

  /**
   * Unwraps a <code>scala.Function1</code>.
   */
  implicit def Function1ScalaFunction1[T, R](f: Function1[T, R]) = f(_)
}
