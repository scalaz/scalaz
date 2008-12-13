package scalaz.control

/**
 * A categorical monoid.
 *
 * <p>
 * All monoid instances must satisfy the semigroup law and 2 additional laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a. append(zero, a) == a</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. append(a, zero) == a</code></li>
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Monoid[M] extends Zero[M] with Semigroup[M]

/**
 * Functions over monoids.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Monoid {
  /**
   * Construct a monoid from the given zero and semigroup. These must satisfy the monoid laws.
   */
  def monoid[M](implicit z: Zero[M], s: Semigroup[M]) = new Monoid[M] {
    val zero = z.zero
    def append(s1: => M, s2: => M) = s append(s1, s2)
  }

  /**
   * A monoid for <code>Predef.String</code>.
   */
  implicit val StringMonoid = monoid[String]

  /**
   * A monoid for integer addition.
   */
  implicit val IntAdditionMonoid = monoid[Int](Zero.IntAdditionZero, Semigroup.IntAdditionSemigroup)

  /**
   * A monoid for integer multiplication.
   */
  implicit val IntMultiplicationMonoid = monoid[Int](Zero.IntMultiplicationZero, Semigroup.IntMultiplicationSemigroup)

  /**
   * A monoid for boolean disjunction.
   */
  implicit val BooleanDisjunctionMonoid = monoid[Boolean](Zero.BooleanDisjunctionZero, Semigroup.BooleanDisjunctionSemigroup)

  /**
   * A monoid for boolean conjunction.
   */
  implicit val BooleanConjunctionMonoid = monoid[Boolean](Zero.BooleanConjunctionZero, Semigroup.BooleanConjunctionSemigroup)

  /**
   * A monoid for <code>forall T. scala.Option[T]</code>.
   */
  implicit def OptionMonoid[A] = monoid[Option[A]]

  /**
   * A monoid for <code>forall T. scala.List[T]</code>.
   */
  implicit def ListMonoid[A] = monoid[List[A]]

  /**
   * A monoid for <code>forall T. scala.Stream[T]</code>.
   */
  implicit def StreamMonoid[A] = monoid[Stream[A]]
  /**
   * A monoid for <code>forall T. scala.Array[T]</code>.
   */
  implicit def ArrayMonoid[A] = monoid[Array[A]]

  /**
   * A monoid for <code>forall T U. scala.Function1[T, U]</code>.
   */
  implicit def Function1Monoid[A, B](implicit s: Monoid[B]) = monoid[A => B]

  /**
   * A monoid for <code>forall T U. scala.Either[T, U]</code>.
   */
  implicit def EitherMonoid[A, B](implicit s: Monoid[A]) = monoid[Either[A, B]](Zero.EitherZero, Semigroup.EitherSemigroup)

  /**
   * A monoid for <code>forall T U. scala.Either[U, T]</code>.
   */
  implicit def FlipEitherMonoid[A, B](implicit s: Monoid[A]) = monoid[Either[B, A]](Zero.FlipEitherZero, Semigroup.FlipEitherSemigroup)

  /**
   * A monoid for <code>forall T U. scala.Either.LeftProjection[U, T]</code>.
   */
  implicit def EitherLeftMonoid[A, B](implicit s: Monoid[A]) = monoid[Either.LeftProjection[A, B]]

  /**
   * A monoid for <code>forall T U. scala.Either.RightProjection[T, U]</code>.
   */
  implicit def EitherRightMonoid[A, B](implicit s: Monoid[A]) = monoid[Either.RightProjection[B, A]]

  /**
   * A monoid for <code>forall M. scalaz.control.Pure[M]</code>.
   */
  def PureMonoid[M[_]](implicit m: Monad[M]) = monoid[Pure[M]](Zero.PureZero[M], Semigroup.PureSemigroup[M])
}
