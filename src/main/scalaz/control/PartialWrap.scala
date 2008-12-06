package scalaz.control

/**
 * Used to partially apply a higher-kinded argument when wrapping control constructs.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait PartialWrap[T[_], U[_[_]], V[_[_], _]] {
  /**
   * Completes the application with inference.
   */
  def apply[A](a: T[A])(implicit t: U[T]): V[T, A]
}
