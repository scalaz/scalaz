package scalaz.control

/**
 * Projects a value into an environment.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Pure[P[_]] {
  /**
   * Project the given value into the environment that is abstracted over.
   */
  def pure[A](a: A): P[A]
}

/**
 * Functions over pure values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Pure {
  /**
   * A pure for identity.
   */
  implicit val IdPure = new Pure[Tuple1] {
    def pure[A](a: A) = Tuple1(a)
  }

  /**
   * A pure for <code>scala.Option</code>.
   */
  implicit val OptionPure = new Pure[Option] {
    def pure[A](a: A) = Some(a)
  }

  /**
   * A pure for <code>scala.Option</code>.
   */
  implicit val ListPure = new Pure[List] {
    def pure[A](a: A) = List(a)
  }

  import list.NonEmptyList
  import list.NonEmptyList.nel

  /**
   * A pure for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListPure = new Pure[NonEmptyList] {
    def pure[A](a: A) = nel(a)
  }

  /**
   * A pure for <code>scala.Stream</code>.
   */
  implicit val StreamPure = new Pure[Stream] {
    def pure[A](a: A) = Stream(a)
  }

  /**
   * A pure for <code>scala.Array</code>.
   */
  implicit val ArrayPure = new Pure[Array] {
    def pure[A](a: A) = Array.make(1, a)
  }

  /**
   * A pure for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Pure[X] = new Pure[PartialType[Function1, X]#Apply] {
    def pure[A](a: A) = (x: X) => a
  }

  /**
   * A pure for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherPure[X] = new Pure[PartialType[Either, X]#Apply] {
    def pure[A](a: A) = Right(a)
  }

  /**
   * A pure for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherPure[X] = new Pure[PartialType[Either, X]#Flip] {
    def pure[A](a: A) = Left(a)
  }

  /**
   * A pure for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftPure[X] = new Pure[PartialType[Either.LeftProjection, X]#Flip] {
    def pure[A](a: A) = Left(a).left
  }

  /**
   * A pure for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightPure[X] = new Pure[PartialType[Either.RightProjection, X]#Apply] {
    def pure[A](a: A) = Right(a).right
  }

  import validation.Validation
  import validation.Validation.success

  /**
   * A pure for <code>forall T. scalaz.validation.Validation[T, ?]</code>.
   */
  implicit def ValidationPure[X] = new Pure[PartialType[Validation, X]#Apply] {
    def pure[A](a: A) = success[X](a)
  }

  import State.value

  /**
   * A pure for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StatePure[X] = new Pure[PartialType[State, X]#Apply] {
    def pure[A](a: A) = value[X](a)
  }

  /**
   * Returns the given value in a pure environment.
   */
  def pure[P[_]] = new {
    def apply[A](a: A)(implicit p: Pure[P]) = p.pure(a)
  }
}

