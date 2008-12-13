package scalaz.control

/**
 * A monad with empty and plus.
 *
 * <p>
 * All monad-empty-plus instances must satisfy the monad laws, the monad-empty laws and the monad-plus law.
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadEmptyPlus[M[_]] extends MonadPlus[M] with MonadEmpty[M]

/**
 * Functions over monad-empty-plus values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadEmptyPlus {
  /**
   * Construct a monad-empty-plus from the given monad-plus and monad-empty. These must satisfy the monad-empty-plus laws.
   */
  def monadEmptyPlus[M[_]](implicit p: MonadPlus[M], e: MonadEmpty[M]) = new MonadEmptyPlus[M] {
    def pure[A](a: A) = p.pure(a)
    def bind[A, B](f: A => M[B], a: M[A]) = p.bind(f, a)
    def plus[A](m1: => M[A], m2: => M[A]) = p.plus(m1, m2)
    def empty[A] = e.empty
  }

  /**
   * A monad-empty-plus for <code>scala.Option</code>.
   */
  implicit val OptionMonadEmptyPlus = monadEmptyPlus[Option]
  /**
   * A monad-empty-plus for <code>scala.Option</code>.
   */
  implicit val ListMonadEmptyPlus = monadEmptyPlus[List]
  /**
   * A monad-empty-plus for <code>scala.Stream</code>.
   */
  implicit val StreamMonadPlus = monadEmptyPlus[Stream]

  /**
   * A monad-empty-plus for <code>scala.Array</code>.
   */
  implicit val ArrayMonadEmptyPlus = monadEmptyPlus[Array]

  /**
   * Constructs a container using the given function and begin value.
   *
   * @param f The function to unfold to construct the container.
   * @param b The beginning value to start unfolding with.
   */
  def unfold[M[_]]: UnfoldApply[M] = new UnfoldApply[M] {
    def apply[A, B](f: B => Option[(A, B)], b: B)(implicit m: MonadEmptyPlus[M]): M[A] =
      f(b) match {
        case None => m.empty
        case Some((a, b)) => m.plus(m.pure(a), unfold[M](f, b))
      }
  }

  /**
   * Provides partial application of type arguments to <code>unfold</code>.
   */
  trait UnfoldApply[M[_]] {
    def apply[A, B](f: B => Option[(A, B)], b: B)(implicit m: MonadEmptyPlus[M]): M[A]
  }

  /**
   * Replicates the given value the given number of times.
   *
   * @param n The number of times to replicate the given value.
   * @param a The value to replicate.
   */
  def replicate[M[_]]: ReplicateApply[M] = new ReplicateApply[M] {
    def apply[A](n: Int, a: A)(implicit m: MonadEmptyPlus[M]): M[A] =
      if(n <= 0) m.empty
      else m.plus(m.pure(a), replicate[M](n - 1, a))
  }

  /**
   * Provides partial application of type arguments to <code>replicate</code>.
   */
  trait ReplicateApply[M[_]] {
    def apply[A](n: Int, a: A)(implicit m: MonadEmptyPlus[M]): M[A]
  }

  /**
   * Sums the given iterable by sequencing.
   */
  def sum[F[_], M[_]] = new SumApply[F, M] {
    def apply[A](as: F[M[A]])(implicit f: FoldRight[F], m: MonadEmptyPlus[M]) =
      f.foldRight[M[A], M[A]](as, m.empty, m.plus(_, _))
  }

  /**
   * Provides partial application of type arguments to <code>replicate</code>.
   */
  trait SumApply[F[_], M[_]] {
    def apply[A](as: F[M[A]])(implicit f: FoldRight[F], m: MonadEmptyPlus[M]): M[A]
  }
}
