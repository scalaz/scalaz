package scalaz.control

/**
 * A monad that also has an empty value.
 *
 * <p>
 * All monad-empty instances must satisfy the monad laws and 2 additional laws:
 * <ol>
 * <li><strong>left identity (empty)</strong><br/><code>forall f. empty == bind(f, empty)</code></li>
 * <li><strong>right identity (empty)</strong><br/><code>forall a. empty == bind(x => a, empty)</code></li>
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadEmpty[M[_]] extends Monad[M] with Empty[M]

/**
 * Functions over monad-empty values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadEmpty {
  /**
   * Construct a monad-empty from the given monad and empty. These must satisfy the monad-empty laws.
   */
  def monadEmpty[M[_]](implicit m: Monad[M], e: Empty[M]) = new MonadEmpty[M] {
    def pure[A](a: A) = m.pure(a)
    def bind[A, B](f: A => M[B], a: M[A]) = m.bind(f, a)
    def empty[A] = e.empty
  }
  
  /**
   * A monad-empty for <code>scala.Option</code>.
   */
  implicit val OptionMonadEmpty = monadEmpty[Option]
  /**
   * A monad-empty for <code>scala.Option</code>.
   */
  implicit val ListMonadEmpty = monadEmpty[List]
  /**
   * A monad-empty for <code>scala.Stream</code>.
   */
  implicit val StreamMonad = monadEmpty[Stream]

  /**
   * A monad-empty for <code>scala.Array</code>.
   */
  implicit val ArrayMonadEmpty = monadEmpty[Array]
}