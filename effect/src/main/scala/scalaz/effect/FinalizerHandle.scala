package scalaz
package effect

/**
 * A handle to a finalizer that allows you to duplicate it to a parent region using "dup".
 * Duplicating a finalizer means that instead of being performed when the current region
 * terminates, it is performed when the parent region terminates.
 */
sealed abstract class FinalizerHandle[R[_]] {
  val finalizer: RefCountedFinalizer
}

object FinalizerHandle {
  def apply[R[_]](r: RefCountedFinalizer): FinalizerHandle[R] = finalizerHandle[R](r)

  def finalizerHandle[R[_]](r: RefCountedFinalizer): FinalizerHandle[R] =
    new FinalizerHandle[R] {
      val finalizer = r
    }
}
