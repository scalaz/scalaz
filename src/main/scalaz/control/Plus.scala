package scalaz.control

/**
 * Appends through an environment.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Plus[P[_]] {
  /**
   * Appends the two given values.
   */
  def plus[A](a1: => P[A], a2: => P[A]): P[A]
}

/**
 * Functions over plus values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Plus {
  /**
   * A plus for <code>scala.Option</code>.
   */
  implicit val OptionPlus = new Plus[Option] {
    def plus[A](a1: => Option[A], a2: => Option[A]) = a1 orElse a2
  }

  /**
   * A plus for <code>scala.List</code>.
   */
  implicit val ListPlus = new Plus[List] {
    def plus[A](a1: => List[A], a2: => List[A]) = a1 ::: a2
  }

  import list.NonEmptyList

  /**
   * A plus for <code>scalaz.control.NonEmptyList</code>.
   */
  implicit val NonEmptyListPlus = new Plus[NonEmptyList] {
    def plus[A](a1: => NonEmptyList[A], a2: => NonEmptyList[A]) = a1.toList <::: a2
  }

  /**
   * A plus for <code>scala.Stream</code>.
   */
  implicit val StreamPlus = new Plus[Stream] {
    def plus[A](a1: => Stream[A], a2: => Stream[A]) = a1 append a2
  }

  /**
   * A plus for <code>scala.Array</code>.
   */
  implicit val ArrayPlus = new Plus[Array] {
    def plus[A](a1: => Array[A], a2: => Array[A]) = a1 ++ a2
  }

  /**
   * A plus for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherPlus[X]: Plus[PartialType[Either, X]#Apply] = new Plus[PartialType[Either, X]#Apply] {
    def plus[A](a1: => Either[X, A], a2: => Either[X, A]) = if(a1.isRight) a1 else a2
  }

  /**
   * A plus for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherPlus[X]: Plus[PartialType[Either, X]#Flip] = new Plus[PartialType[Either, X]#Flip] {
    def plus[A](a1: => Either[A, X], a2: => Either[A, X]) = if(a1.isLeft) a1 else a2
  }

  /**
   * A plus for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftPlus[X]: Plus[PartialType[Either.LeftProjection, X]#Flip] = new Plus[PartialType[Either.LeftProjection, X]#Flip] {
    def plus[A](a1: => Either.LeftProjection[A, X], a2: => Either.LeftProjection[A, X]) = if(a1.e.isRight) a1 else a2
  }

  /**
   * A plus for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightPlus[X]: Plus[PartialType[Either.RightProjection, X]#Apply] = new Plus[PartialType[Either.RightProjection, X]#Apply] {
    def plus[A](a1: => Either.RightProjection[X, A], a2: => Either.RightProjection[X, A]) = if(a1.e.isLeft) a1 else a2
  }
}

/**
 * Wraps <code>Plus</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Plus
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait PlusW[P[_], A] {
  /**
   * The plus value.
   */
  def v: P[A]

  /**
   * The implementation for the plus value.
   */
  val plus: Plus[P]

  /**
   * Plus this environment with the given monad-plus environment.
   */
  final def <+>(a2: => P[A]) = plus.plus(v, a2)
}

/**
 * Functions over plus values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object PlusW {
  /**
   * Constructs a plus from the given value and implementation.
   */
  def plus[P[_]] = new {
    def apply[A](a1: => P[A])(implicit p: Plus[P]) = new PlusW[P, A] {
      def v = a1
      val plus = p
    }
  }

  /**
   * A plus for <code>scala.Option</code>.
   */
  implicit def OptionPlus[A](as: => Option[A]) = plus[Option](as)

  /**
   * A plus for <code>scala.List</code>.
   */
  implicit def ListPlus[A](as: => List[A]) = plus[List](as)

  import list.NonEmptyList

  /**
   * A plus for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListPlus[A](as: => NonEmptyList[A]) = plus[NonEmptyList](as)

  /**
   * A plus for <code>scala.Stream</code>.
   */
  implicit def StreamPlus[A](as: => Stream[A]) = plus[Stream](as)

  /**
   * A plus for <code>scala.Array</code>.
   */
  implicit def ArrayPlus[A](as: => Array[A]) = plus[Array](as)

  /**
   * A plus for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherPlus[A, B](as: => Either[A, B]) = plus[PartialType[Either, A]#Apply](as)

  /**
   * A plus for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherPlus[A, B](as: => Either[B, A]) = plus[PartialType[Either, A]#Flip](as)

  /**
   * A plus for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftPlus[A, B](as: => Either.LeftProjection[B, A]) = plus[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A plus for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightPlus[A, B](as: => Either.RightProjection[A, B]) = plus[PartialType[Either.RightProjection, A]#Apply](as)
}
