package scalaz.control

/**
 * The Kleisli data structure. <code>(Monad m) => a -> m b</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Kleisli[M[_], -A, B] {
  /**
   * Apply the given value to produce a monadic value.
   */
  def apply(a: A): M[B]

  /**
   * An alias for apply.
   */
  def ~>(a: A) = apply(a)

  import Kleisli._

  /**
   * Kleisli composition.
   */
  def >=>[C](k: Kleisli[M, B, C])(implicit m: Monad[M]) = kleisli[M]((a: A) => m.bind(k(_: B), this(a)))

  /**
   * Maps the given function across a kleisli structure.
   */
  def map[C](f: B => C)(implicit ftr: Functor[M]) = kleisli[M]((a: A) => ftr.fmap(f(_: B), this(a)))

  /**
   * Contra-variant map for the given function across a kleisli structure.
   */
  def comap[C](f: C => A): Kleisli[M, C, B] = kleisli[M]((c: C) => this(f(c)))

  /**
   * Composes the given function with the kleisli function to construct a new kleisli structure.
   */
  def compose[N[_]](f: M[B] => N[B]) = kleisli[N]((a: A) => f(this(a)))

  /**
   * Traverse the kleisli structure using the given traversal.
   */
  def traverse[F[+_]](f: F[A])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]] = t.traverse[M, A, B](Kleisli.this(_), f)

  /**
   * Traverse the kleisli structure as a list.
   */
  def traverses(as: List[A])(implicit a: Applicative[M]) = traverse[List](as)
}

/**
 * Kleisli construction functions.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Kleisli {
  /**
   * Permits partial application of type variables for constructing a kleisli structure.
   */
  sealed trait KleisliApply[M[_]] {
    /**
     * Construct a kleisli structure using the given function and monad.
     */
    def apply[A, B](f: A => M[B]): Kleisli[M, A, B]
  }

  /**
   * Constructs a kleisli structure.
   */
  def kleisli[M[_]] = new KleisliApply[M] {
    def apply[A, B](f: A => M[B]) = new Kleisli[M, A, B] {
      def apply(a: A) = f(a)
    }
  }

  /**
   * For lifting a function into a kleisli structure.
   */
  sealed trait KleisliLift[M[_]] {
    def apply[A, B](f: A => B)(implicit p: Pure[M]): Kleisli[M, A, B]
  }

  /**
   * Constructs a kleisli structure by lifting the given function.
   */
  def lift[M[_]] = new KleisliLift[M] {
    def apply[A, B](f: A => B)(implicit p: Pure[M]) = kleisli[M]((a: A) => p.pure(f(a)))
  }

  /**
   * Creates a kleisli structure that returns the argument in <code>Some</code> if the given predicate satisfies on the
   * argument.  
   */
  def filter[A](f: A => Boolean): Kleisli[Option, A, A] = kleisli[Option]((a: A) => if(f(a)) Some(a) else None)
}
