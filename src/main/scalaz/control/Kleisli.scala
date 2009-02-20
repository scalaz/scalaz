package scalaz.control

/**
 * The Kleisli data structure. <code>(Monad m) => a -> m b</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Kleisli[M[+_], -A, +B] {
  /**
   * Apply the given value to produce a monadic value.
   */
  def apply(a: A): M[B]

  /**
   * The monad instance for this kleisli structure.
   */
  val monad: Monad[M]

  import Kleisli._

  /**
   * Kleisli composition.
   */
  def >=>[C](k: Kleisli[M, B, C]): Kleisli[M, A, C] = kleisli[M]((a: A) => monad.bind(k(_: B), this(a)))(monad)
}

/**
 * Kleisli construction functions.
 *
 * @see Cofunctor
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Kleisli {
  /**
   * Permits partial application of type variables for constructing a kleisli structure.
   */
  trait KleisliApply[M[+_]] {
    /**
     * Construct a kleisli structure using the given function and monad.
     */
    def apply[A, B](f: A => M[B])(implicit m: Monad[M]): Kleisli[M, A, B]
  }

  /**
   * Constructs a kleisli structure.
   */
  def kleisli[M[+_]] = new KleisliApply[M] {
    def apply[A, B](f: A => M[B])(implicit m: Monad[M]) = new Kleisli[M, A, B] {
      def apply(a: A) = f(a)
      val monad = m       
    }
  }
}
