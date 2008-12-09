package scalaz.control

/**
 * Function application within an environment.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Apply[AP[_]] {
  def apply[A, B](f: AP[A => B], fa: AP[A]): AP[B]
}

/**
 * Functions over apply values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Apply {
  /**
   * An apply for identity.
   */
  implicit val IdApply = new Apply[Tuple1] {
    def apply[A, B](f: Tuple1[A => B], a: Tuple1[A]) = Tuple1(f._1(a._1))
  }

  /**
   * An apply for <code>scala.Option</code>.
   */
  implicit val OptionApply = new Apply[Option] {
    def apply[A, B](f: Option[A => B], a: Option[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An apply for <code>scala.List</code>.
   */
  implicit val ListApply = new Apply[List] {
    def apply[A, B](f: List[A => B], a: List[A]) = f flatMap (f => a map(f(_)))
  }

  import list.NonEmptyList

  /**
   * An applicative functor for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListApply = new Apply[NonEmptyList] {
    def apply[A, B](f: NonEmptyList[A => B], a: NonEmptyList[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An apply for <code>scala.Stream</code>.
   */
  implicit val StreamApply = new Apply[Stream] {
    def apply[A, B](f: Stream[A => B], a: Stream[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An apply for <code>scala.Array</code>.
   */
  implicit val ArrayApply = new Apply[Array] {
    def apply[A, B](f: Array[A => B], a: Array[A]) = f flatMap (f => a map(f(_)))
  }

  /**
   * An apply for <code>scala.Function1[T, ?]</code>.
   */
  implicit def Function1Apply[X] = new Apply[PartialType[Function1, X]#Apply] {
    def apply[A, B](f: X => A => B, a: X => A) = (x: X) => f(x)(a(x))
  }

  /**
   * An apply for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherApply[X] = new Apply[PartialType[Either, X]#Apply] {
    def apply[A, B](f: Either[X, A => B], a: Either[X, A]) = f.right.flatMap(f => a.right.map(f(_)))
  }

  /**
   * An apply for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherApply[X] = new Apply[PartialType[Either, X]#Flip] {
    def apply[A, B](f: Either[A => B, X], a: Either[A, X]) = f.left.flatMap(f => a.left.map(f(_)))
  }

  /**
   * An apply for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftApply[X] = new Apply[PartialType[Either.LeftProjection, X]#Flip] {
    def apply[A, B](f: Either.LeftProjection[A => B, X], a: Either.LeftProjection[A, X]) = f.flatMap(f => a.map(f(_))).left
  }

  /**
   * An apply for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightApply[X] = new Apply[PartialType[Either.RightProjection, X]#Apply] {
    def apply[A, B](f: Either.RightProjection[X, A => B], a: Either.RightProjection[X, A]) = f.flatMap(f => a.map(f(_))).right
  }

  import validation.Validation
  import validation.Validation.{success, fail}
  import SemigroupW._

  /**
   * An apply for <code>forall T. scalaz.validation.Validation[T, ?]</code>.
   */
  implicit def ValidationApply[X](implicit s: Semigroup[X]) = new Apply[PartialType[Validation, X]#Apply] {
    def apply[A, B](f: Validation[X, A => B], a: Validation[X, A]) =
      (f.either, a.either) match {
        case (Right(f), Right(a)) => success[X](f(a))
        case (Right(_), Left(e)) => fail[B](e)
        case (Left(e), Right(_)) => fail[B](e)
        case (Left(e1), Left(e2)) => fail[B](e1 |+| e2)
      }
  }

  /**
   * An apply for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateApply[X] = new Apply[PartialType[State, X]#Apply] {
    def apply[A, B](f: State[X, A => B], a: State[X, A]) = f.flatMap(f => a.map(f(_)))
  }
}

/**
 * Wraps <code>Apply</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Apply
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait ApplyW[AP[_], A] {
  /**
   * The apply value.
   */
  val v: AP[A]

  /**
   * The apply implementation for the value.
   */
  val apply: Apply[AP]

  /**
   * Applies the given applicative function to this applicative instance.
   */
  final def <*>[B](f: AP[A => B]) = apply(f, v)

  /**
   * Applies the given applicative function to this applicative instance.
   */
  final def <*>:[B](f: AP[A => B]) = <*>(f)
}

/**
 * Functions over apply values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ApplyW {
  /**
   * Constructs an apply from the given value and implementation.
   */
  def apply[AP[_]] = new PartialWrap[AP, Apply, ApplyW] {
    def apply[A](ap: AP[A])(implicit a: Apply[AP]) = new ApplyW[AP, A] {
      val v = ap
      val apply = a
    }
  }

  /**
   * An apply for identity.
   */
  implicit def IdApply[A](as: Tuple1[A]) = apply[Tuple1](as)

  /**
   * An apply for <code>scala.Option</code>.
   */
  implicit def OptionApply[A](as: Option[A]) = apply[Option](as)

  /**
   * An apply for <code>scala.List</code>.
   */
  implicit def ListApply[A](as: List[A]) = apply[List](as)

  import list.NonEmptyList

  /**
   * An apply for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListApply[A](as: NonEmptyList[A]) = apply[NonEmptyList](as)

  /**
   * An apply for <code>scala.Stream</code>.
   */
  implicit def StreamApply[A](as: Stream[A]) = apply[Stream](as)

  /**
   * An apply for <code>scala.Array</code>.
   */
  implicit def ArrayApply[A](as: Array[A]) = apply[Array](as)

  /**
   * An apply for <code>forall T. scala.Function[T, ?]</code>.
   */
  implicit def Function1Apply[A, B](as: A => B) = apply[PartialType[Function1, A]#Apply](as)

  /**
   * An apply for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherApply[A, B](as: Either[A, B]) = apply[PartialType[Either, A]#Apply](as)

  /**
   * An apply for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherApply[A, B](as: Either[B, A]) = apply[PartialType[Either, A]#Flip](as)

  /**
   * An apply for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftApply[A, B](as: Either.LeftProjection[B, A]) = apply[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * An apply for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightApply[A, B](as: Either.RightProjection[A, B]) = apply[PartialType[Either.RightProjection, A]#Apply](as)

  import validation.Validation

  /**
   * An apply for <code>forall T. scalaz.validation.Validation[T, ?]</code>.
   */
  implicit def ValidationApply[A, B](as: Validation[A, B])(implicit s: Semigroup[A]) = apply[PartialType[Validation, A]#Apply](as)
}
