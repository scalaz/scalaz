package scalaz.control

/**
 * Covariant function application in an environment. i.e. a covariant Functor.
 *
 * <p>
 * All functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == fmap(identity, a)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. fmap(f compose g, a) == fmap(f, fmap(g, a))</code></li>
 * </ol>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Functor[F[_]] {
  /**
   * Maps the given function across the given environement.
   */
  def fmap[A, B](f: A => B, fa: F[A]): F[B]
}

/**
 * Functions over covariant functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Functor {
  /**
   * A covariant functor for identity.
   */
  implicit val IdFunctor = new Functor[Tuple1] {
    def fmap[A, B](f: A => B, fa: Tuple1[A]) = Tuple(f(fa._1))
  }

  /**
   * A covariant functor for <code>scala.Option</code>.
   */
  implicit val OptionFunctor = new Functor[Option] {
    def fmap[A, B](f: A => B, fa: Option[A]) = fa map f
  }

  /**
   * A covariant functor for <code>scala.List</code>.
   */
  implicit val ListFunctor = new Functor[List] {
    def fmap[A, B](f: A => B, fa: List[A]) = fa map f
  }

  import list.NonEmptyList

  /**
   * A covariant functor for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListFunctor = new Functor[NonEmptyList] {
    def fmap[A, B](f: A => B, fa: NonEmptyList[A]) = fa map f
  }

  /**
   * A covariant functor for <code>scala.Stream</code>.
   */
  implicit val StreamFunctor = new Functor[Stream] {
    def fmap[A, B](f: A => B, fa: Stream[A]) = fa map f
  }

  /**
   * A covariant functor for <code>scala.Array</code>.
   */
  implicit val ArrayFunctor = new Functor[Array] {
    def fmap[A, B](f: A => B, fa: Array[A]) = fa map f
  }

  /**
   * A covariant functor for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Functor[X] = new Functor[PartialType[Function1, X]#Apply] {
    def fmap[A, B](f: A => B, fa: X => A): X => B = fa andThen f
  }

  /**
   * A covariant functor for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherFunctor[X] = new Functor[PartialType[Either, X]#Apply] {
    def fmap[A, B](f: A => B, fa: Either[X, A]) = fa.right map f
  }

  /**
   * A covariant functor for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherFunctor[X] = new Functor[PartialType[Either, X]#Flip] {
    def fmap[A, B](f: A => B, fa: Either[A, X]) = fa.left map f
  }

  /**
   * A covariant functor for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def EitherLeftFunctor[X] = new Functor[PartialType[Either.LeftProjection, X]#Flip] {
    def fmap[A, B](f: A => B, fa: Either.LeftProjection[A, X]) = fa.map(f).left
  }

  /**
   * A covariant functor for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherRightFunctor[X] = new Functor[PartialType[Either.RightProjection, X]#Apply] {
    def fmap[A, B](f: A => B, fa: Either.RightProjection[X, A]) = fa.map(f).right
  }

  import validation.Validation

  /**
   * A covariant functor for <code>forall T. scalaz.validaton.Validation[T, ?]</code>.
   */
  implicit def ValidationFunctor[X] = new Functor[PartialType[Validation, X]#Apply] {
    def fmap[A, B](f: A => B, fa: Validation[X, A]): Validation[X, B] = fa.map(f)
  }

  /**
   * A covariant functor for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateFunctor[S]: Functor[PartialType[State, S]#Apply] = new Functor[PartialType[State, S]#Apply] {
    def fmap[A, B](f: A => B, fa: State[S, A]) = fa map f
  }
}

/**
 * Wraps <code>Functor</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Functor
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FunctorW[F[_], A] {
  /**
   * The functor value.
   */
  val v: F[A]

  /**
   * The implementation for the functor value.
   */
  val functor: Functor[F]

  /**
   * Maps the given function across this functor.
   */
  final def >[B](f: A => B) = functor fmap (f, v)

  /**
   * Maps the given function across this functor.
   */
  final def <-:[B](f: A => B) = >(f)
}

/**
 * Functions over functors.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FunctorW {
  /**
   * Constructs a functor from the given value and implementation.
   */
  def functor[F[_]] = new PartialWrap[F, Functor, FunctorW] {
    def apply[A](ft: F[A])(implicit f: Functor[F]) = new FunctorW[F, A] {
      val v = ft
      val functor = f
    }
  }

  /**
   * A functor for identity.
   */
  implicit def IdFunctor[A](as: Tuple1[A]) = functor[Tuple1](as)

  /**
   * A functor for <code>scala.Option</code>.
   */
  implicit def OptionFunctor[A](as: Option[A]) = functor[Option](as)

  /**
   * A functor for <code>scala.List</code>.
   */
  implicit def ListFunctor[A](as: List[A]) = functor[List](as)

  import list.NonEmptyList

  /**
   * A functor for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListFunctor[A](as: NonEmptyList[A]) = functor[NonEmptyList](as)

  /**
   * A functor for <code>scala.Stream</code>.
   */
  implicit def StreamFunctor[A](as: Stream[A]) = functor[Stream](as)

  /**
   * A functor for <code>scala.Array</code>.
   */
  implicit def ArrayFunctor[A](as: Array[A]) = functor[Array](as)

  /**
   * A functor for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Functor[A, B](as: A => B) = functor[PartialType[Function1, A]#Apply](as)

  /**
   * A functor for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherFunctor[A, B](as: Either[A, B]) = functor[PartialType[Either, A]#Apply](as)

  /**
   * A functor for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherFunctor[A, B](as: Either[B, A]) = functor[PartialType[Either, A]#Flip](as)

  /**
   * A functor for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftFunctor[A, B](as: Either.LeftProjection[B, A]) = functor[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A functor for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightFunctor[A, B](as: Either.RightProjection[A, B]) = functor[PartialType[Either.RightProjection, A]#Apply](as)

  import validation.Validation

  /**
   * A functor for <code>forall T. scalaz.validation.Validation[T, ?]</code>.
   */
  implicit def ValidationFunctor[A, B](as: Validation[A, B]) = functor[PartialType[Validation, A]#Apply](as)

  /**
   * A functor for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateFunctor[A, B](as: State[A, B]) = functor[PartialType[State, A]#Apply](as)
}
