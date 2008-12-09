package scalaz.control

/**
 * Binary functor.bimap (f ◦ h) (g ◦ k) = bimap f g ◦ bimap h k -- composition
 *
 * <p>
 * All binary functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == bimap(identity, identity, a)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g h i. bimap(f compose g, h compose i, a) == bimap(f, h, bimap(g, i, a))</code></li>
 * </ol>
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Bifunctor[F[_, _]] {
  /**
   * Maps the given functions over this binary functor.
   */
  def bimap[A, B, X, Y](f: A => X, g: B => Y, ft: F[A, B]): F[X, Y]
}

/**
 * Functions over binary functors.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Bifunctor {
  /**
   * A binary functor for identity.
   */
  implicit val IdBifunctor = new Bifunctor[Tuple2] {
    def bimap[A, B, X, Y](f: A => X, g: B => Y, ft: (A, B)) = (f(ft._1), g(ft._2))
  }

  /**
   * A binary functor for <code>scala.Either</code>.
   */
  implicit val EitherBifunctor = new Bifunctor[Either] {
    def bimap[A, B, X, Y](f: A => X, g: B => Y, ft: Either[A, B]) = ft match {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }

  import validation.Validation
  import validation.Validation.{success, fail}

  /**
   * A binary functor for <code>scalaz.validation.Validation</code>.
   */
  implicit val ValidationBifunctor = new Bifunctor[Validation] {
    def bimap[A, B, X, Y](f: A => X, g: B => Y, ft: Validation[A, B]) = ft.either match {
      case Left(a) => fail[Y](f(a))
      case Right(b) => success[X](g(b))
    }
  }
}

/**
 * Wraps <code>Bifunctor</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Bifunctor
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait BifunctorW[F[_, _], A, B] {
  /**
   * The bifunctor value.
   */
  val v: F[A, B]

  /**
   * The implementation for the bifunctor value.
   */
  val bifunctor: Bifunctor[F]

  /**
   * Maps the given functions across this bifunctor.
   */
  final def <|>[X, Y](f: A => X, g: B => Y) = bifunctor.bimap(f, g, v)

  /**
   * Maps the given function across one side of this bifunctor.
   */
  final def <|[X](f: A => X) = <|>(f, identity[B])

  /**
   * Maps the given function across one side of this bifunctor.
   */
  final def |>[Y](g: B => Y) = <|>(identity[A], g)
}

/**
 * Functions over bifunctor values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object BifunctorW {
  /**
   * Used to partially apply a higher-kinded argument when wrapping bifunctor.
   *
   * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  trait PartialWrap[T[_, _], U[_[_, _]], V[_[_, _], _, _]] {
    /**
     * Completes the application with inference.
     */
    def apply[A, B](a: T[A, B])(implicit t: U[T]): V[T, A, B]
  }

  /**
   * Constructs a bifunctor from the given value and implementation.
   */
  def bifunctor[F[_, _]] = new PartialWrap[F, Bifunctor, BifunctorW] {
    def apply[A, B](ft: F[A, B])(implicit f: Bifunctor[F]) = new BifunctorW[F, A, B] {
      val v = ft
      val bifunctor = f
    }
  }

  /**
   * A bifunctor for identity.
   */
  implicit def IdBifunctor[A, B](as: (A, B)) = bifunctor[Tuple2](as)

  /**
   * A bifunctor for <code>scala.Either</code>.
   */
  implicit def EitherBifunctor[A, B](as: Either[A, B]) = bifunctor[Either](as)

  import validation.Validation

  /**
   * A bifunctor for <code>scalaz.validation.Validation</code>.
   */
  implicit def ValidationBifunctor[A, B](as: Validation[A, B]) = bifunctor[Validation](as)
}
