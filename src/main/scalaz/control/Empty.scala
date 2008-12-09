package scalaz.control

/**
 * An empty environment.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Empty[E[_]] {
  /**
   * Returns the empty environment.
   */
  def empty[A]: E[A]
}

/**
 * Functions over an empty environment.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Empty {
  /**
   * An empty for <code>scala.Option</code>.
   */
  implicit val OptionEmpty = new Empty[Option] {
    def empty[A] = None
  }

  /**
   * An empty for <code>scala.List</code>.
   */
  implicit val ListEmpty = new Empty[List] {
    def empty[A] = Nil
  }

  /**
   * An empty for <code>scala.Stream</code>.
   */
  implicit val StreamEmpty = new Empty[Stream] {
    def empty[A] = Stream.empty
  }

  /**
   * An empty for <code>scala.Array</code>.
   */
  implicit val ArrayEmpty = new Empty[Array] {
    def empty[A] = new Array(0)
  }

  /**
   * Returns an empty value.
   */
  def empty[E[_], A](implicit e: Empty[E]) = e.empty[A]
}
