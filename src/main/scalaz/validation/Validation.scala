package scalaz.validation

import control.{Semigroup, Zero}
import list.NonEmptyList

/**
 * A wrapper around <code>scala.Either</code> that has renamed identifiers (e.g. <code>Left -> Fail</code>,
 * <code>Right -> Success</code>) for validation. Errors are stored on the <code>Left</code> side of the
 * underlying <code>Either</code>. This type also has a different <code>Applicative</code> instance to
 * <code>Either</code> in that errors are accumulated on the left (should one or more be encountered).
 *
 * @see scala.Either
 * @see scalaz.control.Applicative
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Validation[+E, +A] {
  val either: Either[E, A]

  /**
   * Returns <code>true</code> if this validation has failed, otherwise <code>false</code>.
   */
  val isFail = either.isLeft

  /**
   * Returns <code>false</code> if this validation has failed, otherwise <code>true</code>.
   */
  val isSuccess = either.isRight

  /**
   * A projection of this validation's failure.
   */
  val fail = new Validation.FailProjection[E, A](this)

  /**
   * Returns the success value, or throws an error if there is no success value.
   */
  def success: A = success("Validation: success on fail value")

  /**
   * Returns a failing projection of this validation.
   */
  def fail(e: => String) = either match {
    case Left(a) => a
    case Right(_) => error(e)
  }

  /**
   * Returns the success value or fails with the given error message.
   */
  def success(e: => String) = either match {
    case Left(_) => error(e)
    case Right(b) => b
  }

  /**
   * Puts the potential error in a non-empty list. This is useful for accumulating errors through multiple validations.
   */
  def nel: Validation[NonEmptyList[E], A] = either.left.map(NonEmptyList.nel(_))

  /**
   * Swaps the error and value of this validation.
   */
  def unary_~ : Validation[A, E] = either swap

  /**
   * Maps the two given functions across either side of validation.
   */
  def bimap[X, Y](f: E => X, g: A => Y): Validation[X, Y] = either match {
    case Left(e) => Left(f(e))
    case Right(a) => Right(g(a))
  }

  /**
   * Returns the success value or the given value.
   */
  def |[AA >: A](a: => AA) = either match {
    case Left(_) => a
    case Right(a) => a
  }

  /**
   * Maps the given function across the success side of a validation.
   */
  def map[B](f: A => B): Validation[E, B] = either.right map f

  /**
   * Binds the given function across the success side of a validation.
   */
  def flatMap[B, EE >: E](f: A => Validation[EE, B]): Validation[EE, B] = either.right flatMap (f(_).either)

  /**
   * Binds anonymously the given value across the success side of a validation.
   */
  def sequence[B, EE >: E](v: Validation[EE, B]) = flatMap(x => v)

  /**
   * Returns <code>None</code> if this is a projection of failure or if the given predicate does not hold for the
   * success value, otherwise, returns a success in <code>Some</code>.
   */
  def filter(f: A => Boolean): Option[Validation[E, A]] = either.right filter f map (x => x)

  /**
   * Function application through the success side of validation.
   */
  def apply[B, EE >: E](v: Validation[EE, A => B]) =
    for(f <- v; x <- this) yield f(x)

  /**
   * Function application on the successful side of this validation, or accumulating the errors on the failing side
   * using the given semigroup should one or more be encountered.
   */
  def accumapply[B, EE >: E](v: Validation[EE, A => B])(implicit s: Semigroup[EE]): Validation[EE, B] =
    either match {
      case Left(e1) => v.either match {
        case Left(e2) => Left(s append(e1, e2))
        case Right(_) => Left(e1)
      }
      case Right(a) => v.either match {
        case Left(e2) => Left(e2)
        case Right(f) => Right(f(a))
      }
    }

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   */
  def accumulate[B, C, EE >: E](vb: Validation[EE, B], f: (A, B) => C)(implicit s: Semigroup[EE]): Validation[EE, C] =
    vb accumapply (map(Function.curried(f)))

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   */
  def accumulate[B, C, D, EE >: E](vb: Validation[EE, B], vc: Validation[EE, C], f: (A, B, C) => D)(implicit s: Semigroup[EE]): Validation[EE, D] =
    vc accumapply (accumulate(vb, (a, b: B) => (c: C) => f(a, b, c)))

  /**
   * Accumulates errors on the failing side of this or any given validation if one or more are encountered, or applies
   * the given function if all succeeded and returns that value on the successful side.
   */
  def accumulate[B, C, D, EEE, EE >: E](vb: Validation[EE, B], vc: Validation[EE, C], vd: Validation[EE, D], f: (A, B, C, D) => EEE)(implicit s: Semigroup[EE]): Validation[EE, EEE] =
    vd accumapply (accumulate(vb, vc, (a, b: B, c: C) => (d: D) => f(a, b, c, d)))

  /**
   * Succeeds if this validation or the given validation succeeds using the given function and/or zero for the
   * succeeding values. If both validations fail, then the given semigroup is used and the returned validation fails.
   */
  def >*<[B, EE >: E, AA >: A](x: Validation[EE, AA], f: (AA, AA) => B)(implicit z: Zero[AA], s: Semigroup[EE]): Validation[EE, B] =
    (either, x.either) match {
      case (Right(a), Right(b)) => Right(f(a, b))
      case (Right(a), Left(_)) => Right(f(a, z.zero))
      case (Left(_), Right(b)) => Right(f(z.zero, b))
      case (Left(a), Left(b)) => Left(s.append(a, b))
    }

  /**
   * Succeeds if this validation or the given validation succeeds. If both validations fail, then the given semigroup is
   * used and the returned validation fails.
   */
  def >*<[EE >: E, AA >: A](x: Validation[EE, AA])(implicit z: Zero[AA], s: Semigroup[EE]): Validation[EE, (AA, AA)] =
    >*<(x, (x: AA, y: AA) => (x, y))

  /**
   * A <code>String</code> representation for validation.
   */
  override def toString = either match {
    case Left(a) => "Fail(" + a + ')'
    case Right(b) => "Success(" + b + ')'
  }
}

/**
 * Functions over validation values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Validation {
  /**
   * Wraps a <code>scala.Either</code>.
   */
  implicit def EitherValidation[E, A](e: Either[E, A]): Validation[E, A] = new Validation[E, A] {
    val either = e
  }

  /**
   * Unwraps a <code>scala.Either</code>.
   */
  implicit def ValidationEither[E, A](v: Validation[E, A]): Either[E, A] = v.either

  /**
   * A failing projection of a validation.
   */
  final case class FailProjection[+E, +A](v: Validation[E, A]) {
    /**
     * Returns the error value or the given value.
     */
    def |[EE >: E](e: => EE) = v.either match {
      case Left(e) => e
      case Right(_) => e
    }

    /**
     * Maps the given function across the failing side of a validation.
     */
    def map[F](f: E => F): Validation[F, A] = v.either.left map f

    /**
     * Binds the given function across the failing side of a validation.
     */
    def flatMap[F, AA >: A](f: E => Validation[F, AA]): Validation[F, AA] = v.either.left flatMap (f(_).either)


    /**
     * Binds anonymously the given value across the failing side of a validation.
     */
    def sequence[F, AA >: A](v: Validation[F, AA]) = flatMap(x => v)

    /**
     * Returns <code>None</code> if this is a projection of success or if the given predicate does not hold for the
     * failing value, otherwise, returns a fail in <code>Some</code>.
     */
    def filter(f: E => Boolean): Option[Validation[E, A]] = v.either.left filter f map (x => x)

    /**
     * Function application through the failing side of validation.
     */
    def apply[F, AA >: A](v: Validation[E => F, AA]): Validation[F, AA] =
      for(f <- v.fail; x <- this) yield f(x)
  }

  /**
   * An extractor that matches a failed validation.
   *
   * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object Fail {
    /**
     * An extractor that returns the failed value if there is one (fails to match otherwise).
     */
    def unapply[E, A](v: Validation[E, A]): Option[E] = v.either match {
      case Left(e) => Some(e)
      case Right(_) => None
    }
  }

  /**
   * An extractor that matches a succeeded validation.
   *
   * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object Success {
    /**
     * An extractor that returns the succeeded value if there is one (fails to match otherwise).
     */
    def unapply[E, A](v: Validation[E, A]): Option[A] = v.either match {
      case Right(a) => Some(a)
      case Left(_) => None
    }
  }

  /**
   * Constructs a validation that has succeeded with the given value.
   */
  def success[E] = new {
    def apply[A](a: A): Validation[E, A] = Right(a)
  }

  /**
   * Constructs a validation that has failed with the given error value.
   */
  def fail[A] = new {
    def apply[E](e: E): Validation[E, A] = Left(e)
  }
}
