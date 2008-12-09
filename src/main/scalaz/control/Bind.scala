package scalaz.control

/**
 * Binds a function through an environment (known also as sequencing).
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Bind[BD[_]] {
  /**
   * Binds the given value with the given value through the environment.
   */
  def bind[A, B](f: A => BD[B], ba: BD[A]): BD[B]
}

/**
 * Functions over bind values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Bind {
  /**
   * A bind for identity.
   */
  implicit val IdBind = new Bind[Tuple1] {
    def bind[A, B](f: A => Tuple1[B], a: Tuple1[A]) = f(a._1)
  }

  /**
   * A bind for <code>scala.Option</code>.
   */
  implicit val OptionBind = new Bind[Option] {
    def bind[A, B](f: A => Option[B], a: Option[A]) = a flatMap f
  }

  /**
   * A bind for <code>scala.List</code>.
   */
  implicit val ListBind = new Bind[List] {
    def bind[A, B](f: A => List[B], a: List[A]) = a flatMap f
  }

  import list.NonEmptyList

  /**
   * A bind for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListBind = new Bind[NonEmptyList] {
    def bind[A, B](f: A => NonEmptyList[B], a: NonEmptyList[A]) = a flatMap f
  }

  /**
   * A bind for <code>scala.Stream</code>.
   */
  implicit val StreamBind = new Bind[Stream] {
    def bind[A, B](f: A => Stream[B], a: Stream[A]) = a flatMap f
  }

  /**
   * A bind for <code>scala.Array</code>.
   */
  implicit val ArrayBind = new Bind[Array] {
    def bind[A, B](f: A => Array[B], a: Array[A]) = a flatMap f
  }

  /**
   * A bind for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Bind[X] = new Bind[PartialType[Function1, X]#Apply] {
    def bind[A, B](f: A => X => B, a: X => A) = (x: X) => f(a(x))(x)
  }

  /**
   * A bind for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherBind[X] = new Bind[PartialType[Either, X]#Apply] {
    def bind[A, B](f: A => Either[X, B], a: Either[X, A]) = a.right flatMap f
  }

  /**
   * A bind for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherBind[X] = new Bind[PartialType[Either, X]#Flip] {
    def bind[A, B](f: A => Either[B, X], a: Either[A, X]) = a.left flatMap f
  }

  /**
   * A bind for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def EitherLeftBind[X] = new Bind[PartialType[Either.LeftProjection, X]#Flip] {
    def bind[A, B](f: A => Either.LeftProjection[B, X], a: Either.LeftProjection[A, X]) = a.flatMap(f(_).e).left
  }

  /**
   * A bind for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherRightBind[X] = new Bind[PartialType[Either.RightProjection, X]#Apply] {
    def bind[A, B](f: A => Either.RightProjection[X, B], a: Either.RightProjection[X, A]) = a.flatMap(f(_).e).right
  }

  /**
   * A bind for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateBind[X] = new Bind[PartialType[State, X]#Apply] {
    def bind[A, B](f: A => State[X, B], a: State[X, A]) = a flatMap f
  }

  /**
   * Removes one level of monadic structure, projecting its bound argument into the outer level of the monad. Equivalent
   * to <code>bind(identity, ma)</code>.
   */
  def join[M[_]] = new {
    def apply[A](ma: M[M[A]])(implicit b: Bind[M]) = b.bind(identity[M[A]], ma)
  }
}

/**
 * Wraps <code>Bind</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Bind
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait BindW[BD[_], A] {
  /**
   * The bind value.
   */
  val v: BD[A]

  /**
   * The implementation for the bind value.
   */
  val bind: Bind[BD]

  /**
   * Binds the given function across this monad.
   */
  final def >>=[B](f: A => BD[B]) = bind.bind(f, v)

  /**
   * Constant bind the given value across this monad.
   */
  final def >>[B](b: BD[B]) = bind.bind((a: A) => b, v)

  /**
   * Binds the given function across this monad.
   */
  final def =<<:[B](f: A => BD[B]) = >>=(f)

  /**
   * Constant bind the given value across this monad.
   */
  final def <<:[B](b: BD[B]) = >>(b)
}

/**
 * Functions over bind values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object BindW {
  /**
   * Constructs a bind from the given value and implementation.
   */
  def bind[B[_]] = new PartialWrap[B, Bind, BindW] {
    def apply[A](b: B[A])(implicit bd: Bind[B]) = new BindW[B, A] {
      val v = b
      val bind = bd
    }
  }

  /**
   * A bind for identity.
   */
  implicit def IdBind[A](as: Tuple1[A]) = bind[Tuple1](as)

  /**
   * A bind for <code>scala.Option</code>.
   */
  implicit def OptionBind[A](as: Option[A]) = bind[Option](as)

  /**
   * A bind for <code>scala.List</code>.
   */
  implicit def ListBind[A](as: List[A]) = bind[List](as)

  import list.NonEmptyList

  /**
   * A bind for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListBind[A](as: NonEmptyList[A]) = bind[NonEmptyList](as)

  /**
   * A bind for <code>scala.Stream</code>.
   */
  implicit def StreamBind[A](as: Stream[A]) = bind[Stream](as)

  /**
   * A bind for <code>scala.Array</code>.
   */
  implicit def ArrayBind[A](as: Array[A]) = bind[Array](as)

  /**
   * A bind for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Bind[A, B](as: A => B) = bind[PartialType[Function1, A]#Apply](as)

  /**
   * A bind for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherBind[A, B](as: Either[A, B]) = bind[PartialType[Either, A]#Apply](as)

  /**
   * A bind for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherBind[A, B](as: Either[B, A]) = bind[PartialType[Either, A]#Flip](as)

  /**
   * A bind for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftBind[A, B](as: Either.LeftProjection[B, A]) = bind[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A bind for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightBind[A, B](as: Either.RightProjection[A, B]) = bind[PartialType[Either.RightProjection, A]#Apply](as)

  /**
   * A bind for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateBind[A, B](as: State[A, B]) = bind[PartialType[State, A]#Apply](as)
}
