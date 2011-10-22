package scalaz
package effect

/**
 * A handle to a finalizer that allows you to duplicate it to a parent region using "dup".
 * Duplicating a finalizer means that instead of being performed when the current region
 * terminates, it is performed when the parent region terminates.
 */
sealed trait FinalizerHandle[R[_]] {
  val finalizer: RefCountedFinalizer
}

object FinalizerHandle extends FinalizerHandles

trait FinalizerHandles {
  def finalizerHandle[R[_]]: RefCountedFinalizer => FinalizerHandle[R] =
    r =>
      new FinalizerHandle[R] {
        val finalizer = r
      }
}
