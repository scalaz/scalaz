package scalaz.control

/**
 * Executes a side-effect through an environment.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Each[E[_]] {
  /**
   * Executes the given side-effect through the given environment.
   */
  def each[A](f: A => Unit, fa: E[A]): Unit
}

/**
 * Functions over each values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Each {
  /**
   * An each for identity.
   */
  implicit val IdEach = new Each[Tuple1] {
    def each[A](f: A => Unit, fa: Tuple1[A]) = f(fa._1)
  }

  /**
   * An each for <code>scala.Option</code>.
   */
  implicit val OptionEach = new Each[Option] {
    def each[A](f: A => Unit, fa: Option[A]) = fa foreach f
  }

  /**
   * An each for <code>scala.List</code>.
   */
  implicit val ListEach = new Each[List] {
    def each[A](f: A => Unit, fa: List[A]) = fa foreach f
  }

  import list.NonEmptyList

  /**
   * An each for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListEach: Each[NonEmptyList] = new Each[NonEmptyList] {
    def each[A](f: A => Unit, fa: NonEmptyList[A]): Unit = fa foreach f
  }

  /**
   * An each for <code>scala.Stream</code>.
   */
  implicit val StreamEach = new Each[Stream] {
    def each[A](f: A => Unit, fa: Stream[A]) = fa foreach f
  }

  /**
   * An each for <code>scala.Array</code>.
   */
  implicit val ArrayEach = new Each[Array] {
    def each[A](f: A => Unit, fa: Array[A]) = fa foreach f
  }

  /**
   * An each for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherEach[X] = new Each[PartialType[Either, X]#Apply] {
    def each[A](f: A => Unit, fa: Either[X, A]) = fa.right foreach f
  }

  /**
   * An each for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherEach[X] = new Each[PartialType[Either, X]#Flip] {
    def each[A](f: A => Unit, fa: Either[A, X]) = fa.left foreach f
  }

  /**
   * An each for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftEach[X] = new Each[PartialType[Either.LeftProjection, X]#Flip] {
    def each[A](f: A => Unit, fa: Either.LeftProjection[A, X]) = fa.e.left foreach f
  }

  /**
   * An each for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightEach[X] = new Each[PartialType[Either.RightProjection, X]#Apply] {
    def each[A](f: A => Unit, fa: Either.RightProjection[X, A]) = fa.e.right foreach f
  }
}

/**
 * Wraps <code>Each</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Each
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait EachW[E[_], A] {
  /**
   * Executes the given side-effect for this each.
   */
  def each(f: A => Unit): Unit
}

/**
 * Functions over each values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object EachW {
  /**
   * Constructs an each from the given value and implementation.
   */
  def each[E[_]] = new PartialWrap[E, Each, EachW] {
    def apply[A](ea: E[A])(implicit e: Each[E]) = new EachW[E, A] {
      def each(f: A => Unit) = e.each(f, ea)
    }
  }

  /**
   * An each for identity.
   */
  implicit def IdEach[A](as: Tuple1[A]) = each[Tuple1](as)

  /**
   * An each for <code>scala.Option</code>.
   */
  implicit def OptionEach[A](as: Option[A]) = each[Option](as)

  /**
   * An each for <code>scala.List</code>.
   */
  implicit def ListEach[A](as: List[A]) = each[List](as)

  /**
   * An each for <code>scala.Stream</code>.
   */
  implicit def StreamEach[A](as: Stream[A]) = each[Stream](as)

  /**
   * An each for <code>scala.Array</code>.
   */
  implicit def ArrayEach[A](as: Array[A]) = each[Array](as)

  /**
   * An each for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherEach[A, B](as: Either[A, B]) = each[PartialType[Either, A]#Apply](as)

  /**
   * An each for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherEach[A, B](as: Either[B, A]) = each[PartialType[Either, A]#Flip](as)

  /**
   * An each for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftEach[A, B](as: Either.LeftProjection[B, A]) = each[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * An each for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightEach[A, B](as: Either.RightProjection[A, B]) = each[PartialType[Either.RightProjection, A]#Apply](as)
}
