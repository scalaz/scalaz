package scalaz.control

/**
 * Appends two elements.
 * 
 * <p>
 * All semigroup instances must satisfy 1 law:
 * <ol>
 * <li><strong>associativity</strong><br/><code>forall a b c. append(a, append(b, c)) == append(append(a, b), c)</code></li>
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Semigroup[S] {
  /**
   * Appends the two given elements.
   */
  def append(s1: => S, s2: => S): S
}

/**
 * Functions over pure values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Semigroup {
  /**
   * Constructs a semigroup from the given function implementation.
   */
  def semigroup[S](f: (=> S) => (=> S) => S) = new Semigroup[S] {
    def append(s1: => S, s2: => S) = f(s1)(s2)
  }

  /**
   * A semigroup for <code>Predef.String</code>.
   */
  implicit val StringSemigroup = new Semigroup[String] {
    def append(s1: => String, s2: => String) = s1 + s2
  }

  /**
   * A semigroup for integer addition.
   */
  implicit val IntAdditionSemigroup: Semigroup[Int] = new Semigroup[Int] {
    def append(s1: => Int, s2: => Int) = s1 + s2
  }

  /**
   * A semigroup for integer multiplication.
   */
  implicit val IntMultiplicationSemigroup: Semigroup[Int] = new Semigroup[Int] {
    def append(s1: => Int, s2: => Int) = s1 + s2
  }

  /**
   * A semigroup for boolean disjunction.
   */
  implicit val BooleanDisjunctionSemigroup = new Semigroup[Boolean] {
    def append(s1: => Boolean, s2: => Boolean) = s1 || s2
  }

  /**
   * A semigroup for boolean conjunction.
   */
  implicit val BooleanConjunctionSemigroup = new Semigroup[Boolean] {
    def append(s1: => Boolean, s2: => Boolean) = s1 && s2
  }

  /**
   * A semigroup for <code>forall T. scala.Option[T]</code>.
   */
  implicit def OptionSemigroup[A] = new Semigroup[Option[A]] {
    def append(s1: => Option[A], s2: => Option[A]) = if(s1.isDefined) s1 else s2
  }

  /**
   * A semigroup for <code>forall T. scala.List[T]</code>.
   */
  implicit def ListSemigroup[A] = new Semigroup[List[A]] {
    def append(s1: => List[A], s2: => List[A]) = s1 ::: s2
  }

  import list.NonEmptyList

  /**
   * A semigroup for <code>forall T. scalaz.list.NonEmptyList[T]</code>.
   */
  implicit def NonEmptyListSemigroup[A] = new Semigroup[NonEmptyList[A]] {
    def append(s1: => NonEmptyList[A], s2: => NonEmptyList[A]) = s1.toList <::: s2
  }

  /**
   * A semigroup for <code>forall T. scala.Stream[T]</code>.
   */
  implicit def StreamSemigroup[A] = new Semigroup[Stream[A]] {
    def append(s1: => Stream[A], s2: => Stream[A]) = s1 append s2
  }

  /**
   * A semigroup for <code>forall T. scala.Array[T]</code>.
   */
  implicit def ArraySemigroup[A] = new Semigroup[Array[A]] {
    def append(s1: => Array[A], s2: => Array[A]) = s1 ++ s2
  }

  /**
   * A semigroup for <code>forall T U. scala.Function1[T, U]</code>.
   */
  implicit def Function1Semigroup[A, B](implicit s: Semigroup[B]) = new Semigroup[A => B] {
    def append(s1: => A => B, s2: => A => B) = x => s append (s1(x), s2(x))
  }

  /**
   * A semigroup for <code>forall T U. scala.Either[T, U]</code>.
   */
  implicit def EitherSemigroup[A, B] = new Semigroup[Either[A, B]] {
    def append(s1: => Either[A, B], s2: => Either[A, B]) = if(s1.isRight) s1 else s2
  }

  /**
   * A semigroup for <code>forall T U. scala.Either[U, T]</code>.
   */
  implicit def FlipEitherSemigroup[A, B] = new Semigroup[Either[B, A]] {
    def append(s1: => Either[B, A], s2: => Either[B, A]) = if(s1.isLeft) s1 else s2
  }

  /**
   * A semigroup for <code>forall T U. scala.Either.LeftProjection[U, T]</code>.
   */
  implicit def EitherLeftSemigroup[A, B] = new Semigroup[Either.LeftProjection[A, B]] {
    def append(s1: => Either.LeftProjection[A, B], s2: => Either.LeftProjection[A, B]) = if(s1.e.isRight) s1 else s2
  }

  /**
   * A semigroup for <code>forall T U. scala.Either.RightProjection[T, U]</code>.
   */
  implicit def EitherRightSemigroup[A, B] = new Semigroup[Either.RightProjection[B, A]] {
    def append(s1: => Either.RightProjection[B, A], s2: => Either.RightProjection[B, A]) = if(s1.e.isLeft) s1 else s2
  }

  /**
   * A semigroup for <code>forall K V. scala.collection.immutable.Map[K, V]</code>.
   */
  implicit def MapSemigroup[K, V] = new Semigroup[Map[K, V]] {
    def append(m1: => Map[K, V], m2: => Map[K, V]) = m1 ++ m2
  }

  import validation.Validation
  import validation.Validation.{success, fail}

  /**
   * A semigroup for <code>forall T U. scalaz.validation.Validation[T, U]</code>.
   */
  implicit def ValidationRightSemigroup[A, B](implicit sa: Semigroup[A], sb: Semigroup[B]) = new Semigroup[Validation[A, B]] {
    def append(s1: => Validation[A, B], s2: => Validation[A, B]) =
      (s1.either, s2.either) match {
        case (Right(a1), Right(a2)) => success[A](sb append(a1, a2))
        case (Right(_), Left(e)) => fail[B](e)
        case (Left(e), Right(_)) => fail[B](e)
        case (Left(e1), Left(e2)) => fail[B](sa append (e1, e2))
      }
  }

  /**
   * A semigroup for <code>forall M. scalaz.control.Pure[M]</code>.
   */
  def PureSemigroup[M[_]](implicit m: Monad[M]) = new Semigroup[Pure[M]] {
    def append(s1: => Pure[M], s2: => Pure[M]) = new Pure[M] {
      def pure[A](a: A) = m.bind(s1.pure(_: A), s2.pure(a))
    }
  }
}

trait SemigroupW[S] {
  def v: S
  val sg: Semigroup[S]

  final def |+|(s2: => S) = sg append (v, s2)
}

object SemigroupW {
  implicit def semigroup[A](a: => A)(implicit s: Semigroup[A]) = new SemigroupW[A] {
    def v = a
    val sg = s
  }
}
