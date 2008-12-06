// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Wraps <code>scala.Either</code> and provides additional methods.
 *
 * @see scala.Either
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait EitherW[+A, +B] {
  /**
   * The value of this either.
   */
  val e: Either[A, B]

  /**
   * A synonym for <code>Either.swap</code>.
   */
  def unary_~ = e.swap

  /**
   * Returns the first argument if this is <code>Left</code>, otherwise the second argument.
   *
   * @param left The value to return if this is <code>Left</code>.
   * @param right The value to return if this is <code>Right</code>.
   */
  def ?[X](left: => X, right: => X) = e match {
    case Left(_) => left
    case Right(_) => right
  }
}

/**
 * Functions over either values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object EitherW {
  /**
   * Wraps a <code>scala.Either</code>.
   */
  implicit def EitherEitherW[A, B](ee: Either[A, B]): EitherW[A, B] = new EitherW[A, B] {
    val e = ee
  }

  /**
   * Unwraps a <code>scala.Either</code>.
   */
  implicit def EitherWEither[A, B](e: EitherW[A, B]): Either[A, B] = e.e

  import EitherW._

  /**
   * Wraps a <code>scala.Either.LeftProjection</code>.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  sealed abstract class LeftProjectionW[+A, +B] {
    /**
     * The value of this left projection.
     */
    val p: Either.LeftProjection[A, B]

    /**
     * The either projected back.
     */
    def e = p.map(identity[A])

    /**
     * Returns the value from this <code>Left</code> or the result of the given argument applied to
     * the value of this <code>Right</code> value.
     *
     * @param f The function that is applied to the <code>Right</code> value.
     */
    def value[AA >: A](f: B => AA) = p.e match {
      case Left(a) => a
      case Right(b) => f(b)
    }

    /**
     * Function application on <code>Left</code>.
     *
     * @param e The Either of the function to apply on the left.
     */
    def apply[X, BB >: B](e: Either[A => X, BB]) =
      e.left.flatMap(f => p.map(f(_)))

    /**
     * Returns the projection value if available or the given value.
     */
    def |[AA >: A](a: => AA) = p.e match {
      case Left(a) => a
      case Right(_) => a
    }
  }

  /**
   * Functions over left projections.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object LeftProjectionW {
    /**
     * Wraps a <code>scala.Either.LeftProjection</code>.
     */
    implicit def LeftProjectionLeftProjectionW[A, B](pp: Either.LeftProjection[A, B]): LeftProjectionW[A, B] = new LeftProjectionW[A, B] {
      val p = pp.e.left
    }

    /**
     * Unwraps a <code>scala.Either.LeftProjection</code>.
     */
    implicit def LeftProjectionWLeftProjection[A, B](p: LeftProjectionW[A, B]): Either.LeftProjection[A, B] = p.p
  }

  /**
   * Wraps a <code>scala.Either.RightProjection</code>.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  sealed abstract class RightProjectionW[+A, +B] {
    /**
     * The value of this right projection.
     */
    val p: Either.RightProjection[A, B]

    /**
     * The either projected back.
     */
    def e = p.map(identity[B])

    /**
     * Returns the value from this <code>Right</code> or the result of the given argument applied to
     * the value of this <code>Left</code> value.
     *
     * @param f The function that is applied to the <code>Left</code> value.
     */
    def value[BB >: B](f: A => BB) = p.e match {
      case Left(a) => f(a)
      case Right(b) => b
    }

    /**
     * Function application on <code>Right</code>.
     *
     * @param e The Either of the function to apply on the right.
     */
    def apply[Y, AA >: A](e: Either[AA, B => Y]) =
      e.right.flatMap(f => p.map(f(_)))

    /**
     * Returns the projection value if available or the given value.
     */
    def |[BB >: B](b: => BB) = p.e match {
      case Left(_) => b
      case Right(b) => b
    }
  }

  /**
   * Functions over right projections.
   *
   * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  object RightProjectionW {
    /**
     * Wraps a <code>scala.Either.RightProjection</code>.
     */
    implicit def RightProjectionRightProjectionW[A, B](pp: Either.RightProjection[A, B]): RightProjectionW[A, B] = new RightProjectionW[A, B] {
      val p = pp.e.right
    }

    /**
     * Unwraps a <code>scala.Either.RightProjection</code>.
     */
    implicit def RightProjectionWRightProjection[A, B](p: RightProjectionW[A, B]): Either.RightProjection[A, B] = p.p
  }

  /**
   * An alternative way to handle an exception that may be thrown in the evaluation of the given
   * argument.
   */
  def throws[B](b: => B) =
    try {
      Right(b)
    } catch {
      case e => Left(e)
    }

  /**
   * Throw the exception if left, otherwise, the right value.
   */
  def throwIt[B](e: EitherW[Throwable, B]) = e fold (throw _, identity[B])

  /**
   * Joins an <code>Either</code> through <code>Left</code>.
   */
  def joinLeft[A, B](es: Either[Either[A, B], B]) =
    es.left.flatMap(x => x)

  /**
   * Joins an <code>Either</code> through <code>Right</code>.
   */
  def joinRight[A, B](es: Either[A, Either[A, B]]) =
    es.right.flatMap(x => x)

  /**
   * Constructs a <code>Left</code> value with an upcast to <code>Either</code>.
   */
  def left[B] = new {
    def apply[A](a: A): Either[A, B] = Left(a)
  }

  /**
   * Constructs a <code>Right</code> value with an upcast to <code>Either</code>.
   */
  def right[A] = new {
    def apply[B](b: B): Either[A, B] = Right(b)
  }

  /**
   * The either inside the right projection.
   */
  implicit def RightProjectionEither[A, B](p: Either.RightProjection[A, B]) = p.e

  /**
   * The either inside the left projection.
   */
  implicit def LeftProjectionEither[A, B](p: Either.LeftProjection[A, B]) = p.e

  /**
   * The either inside the right projection.
   */
  implicit def RightProjectionLeftProjection[A, B](p: Either.RightProjection[A, B]) = p.e.left

  /**
   * The either inside the left projection.
   */
  implicit def LeftProjectionRightProjection[A, B](p: Either.LeftProjection[A, B]) = p.e.right
}

