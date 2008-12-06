package scalaz.control

/**
 * Contra-variant function application in an environment.
 *
 * <p>
 * All contra-variant functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == comap(identity, a)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. comap(f compose g, a) == comap(g, comap(f, a))</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Cofunctor[F[_]] {
  /**
   * Map the given function across the given contra-variant functor.
   */
  def comap[A, B](f: B => A, fa: F[A]): F[B]
}

/**
 * Functions over contra-variant functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Cofunctor {
  /**
   * A contra-variant functor for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Cofunctor[X]: Cofunctor[PartialType[Function1, X]#Flip] = new Cofunctor[PartialType[Function1, X]#Flip] {
    def comap[A, B](f: B => A, fa: A => X) = fa compose f
  }
}

/**
 * Wraps <code>Cofunctor</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Cofunctor
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait CofunctorW[F[_], A] {
  /**
   * The contra-variant value.
   */
  val v: F[A]

  /**
   * The implementation for the contra-variant value.
   */
  val cofunctor: Cofunctor[F]

  /**
   * Maps the given function across this cofunctor.
   */
  final def <[B](f: B => A) = cofunctor.comap(f, v)

  /**
   * Maps the given function across this cofunctor.
   */
  final def ->:[B](f: B => A) = <(f)
}

/**
 * Functions over contra-variant values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object CofunctorW {
  /**
   * Constructs a contra-variant from the given value and implementation.
   */
  def cofunctor[F[_]] = new PartialWrap[F, Cofunctor, CofunctorW] {
    def apply[A](fa: F[A])(implicit f: Cofunctor[F]) = new CofunctorW[F, A] {
      val v = fa
      val cofunctor = f
    }
  }

  /**
   * A contra-variant functor for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Cofunctor[A, B](as: B => A): CofunctorW[PartialType[Function1, A]#Flip, B] = cofunctor[PartialType[Function1, A]#Flip].apply[B](as)
}
