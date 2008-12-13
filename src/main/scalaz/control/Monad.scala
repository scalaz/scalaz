package scalaz.control

/**
 * Abstract a model that sequences computation through an environment.
 *
 * <p>
 * All monad instances must satisfy 3 laws:
 * <ol>
 * <li><strong>left identity</strong><br/><code>forall a f. f(a) == bind(f, unit(a))</code></li>
 * <li><strong>right identity</strong><br/><code>forall a. a == bind(x => unit(x), a)</code></li>
 * <li><strong>associativity</strong><br/><code>forall a f g. bind(x => bind(g, f(x)), a) == bind(g, bind(f, a))</code></li>
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Monad[M[_]] extends Pure[M] with Bind[M] with Functor[M] with Apply[M] {
  /**
   * The functor implementation.
   */
  final def fmap[A, B](f: A => B, fa: M[A]) = bind(pure _ compose f, fa)

  /**
   * The apply implementation.
   */
  final def apply[A, B](f: M[A => B], a: M[A]) = bind((f: A => B) => fmap(f, a), f)
}

/**
 * Functions over monads.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Monad {
  /**
   * Construct a monad from the given pure and bind. These must satisfy the monad laws.
   */
  def monad[M[_]](implicit p: Pure[M], b: Bind[M]) = new Monad[M] {
    def pure[A](a: A) = p.pure(a)
    def bind[A, B](f: A => M[B], a: M[A]) = b.bind(f, a)
  }

  /**
   * A monad for identity.
   */
  implicit val IdMonad = monad[Tuple1]

  /**
   * A monad for <code>scala.Option</code>.
   */
  implicit val OptionMonad = monad[Option]

  /**
   * A monad for <code>scala.Option</code>.
   */
  implicit val ListMonad = monad[List]

  import list.NonEmptyList

  /**
   * A monad for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListMonad = monad[NonEmptyList]

  /**
   * A monad for <code>scala.Stream</code>.
   */
  implicit val StreamMonad = monad[Stream]

  /**
   * A monad for <code>scala.Array</code>.
   */
  implicit val ArrayMonad = monad[Array]

  /**
   * A monad for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Monad[X] = monad[PartialType[Function1, X]#Apply]

  /**
   * A monad for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherMonad[X] = monad[PartialType[Either, X]#Apply]

  /**
   * A monad for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherMonad[X] = monad[PartialType[Either, X]#Flip]

  /**
   * A monad for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftMonad[X] = monad[PartialType[Either.LeftProjection, X]#Flip]

  /**
   * A monad for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightMonad[X] = monad[PartialType[Either.RightProjection, X]#Apply]

  /**
   * A monad for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateMonad[X] = monad[PartialType[State, X]#Apply]

  import SemigroupW._

  /**
   * Sequences the given iterable of monad values to produce a monad value of a container.
   */
  def sequence[M[_], F[_], N[_]] = new SequenceApply[M, F, N] {
    def apply[A](ms: F[M[A]])(implicit m: Monad[M], fr: FoldRight[F], p: Pure[N], mo: Monoid[N[A]]): M[N[A]] =
      fr.foldRight[M[A], M[N[A]]](ms, m.pure(mo.zero), (a, b) => m.bind((x: A) => m.fmap((xs: N[A]) => p.pure(x) |+| xs, b), a))
  }

  /**
   * Provides partial application of type arguments to <code>sequence</code>.
   */
  trait SequenceApply[M[_], F[_], N[_]] {
    def apply[A](ms: F[M[A]])(implicit m: Monad[M], fr: FoldRight[F], p: Pure[N], mo: Monoid[N[A]]): M[N[A]]
  }
}

object T {
  def main(args: Array[String]) {
    import Monad._
    val x = sequence[List, List, List](List(List(1, 2, 3), List(4, 5, 6)))
    println(x)    
  }
}

/**
 * Wraps <code>Monad</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Monad
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadW[M[_], A] extends BindW[M, A] with ApplicativeW[M, A] {
  /**
   * The monad value.
   */
  val v: M[A]

  /**
   * The implementation for the monad value.
   */
  val monad: Monad[M]

  /**
   * Sequence this monad the given number of times.
   */
  def replicateM[T[_]](n: Int)(implicit m: MonadEmptyPlus[T], fr: FoldRight[T], p: Pure[T], mo: Monoid[T[A]]) = {
    implicit val x = monad
    Monad.sequence[M, T, T](MonadEmptyPlus.replicate[T](n, v))
  }

  /**
   * Map the given function across this monad by sequencing.
   */
  def mapM[N[_], O[_]] = new {
    def apply[B](f: A => O[B])(implicit m: Monad[O], fr: FoldRight[M], p: Pure[N], mo: Monoid[N[B]]) =
      Monad.sequence[O, M, N](monad.fmap(f, v))
  }
}

/**
 * Functions over monads.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MonadW {
  /**
   * Constructs a monad from the given value and implementation.
   */
  def monad[M[_]]: PartialWrap[M, Monad, MonadW] = new PartialWrap[M, Monad, MonadW] {
    def apply[A](ma: M[A])(implicit m: Monad[M]) = new MonadW[M, A] {
      val v = ma
      val monad = m
      val bind = m
      val pure = m
      val apply = m
      val functor = m
    }
  }

  /**
   * A monad for identity.
   */
  implicit def IdMonad[A](as: Tuple1[A]) = monad[Tuple1](as)

  /**
   * A monad for <code>scala.Option</code>.
   */
  implicit def OptionMonad[A](as: Option[A]) = monad[Option](as)

  /**
   * A monad for <code>scala.List</code>.
   */
  implicit def ListMonad[A](as: List[A]) = monad[List](as)

  import list.NonEmptyList

  /**
   * A monad for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListMonad[A](as: NonEmptyList[A]) = monad[NonEmptyList](as)

  /**
   * A monad for <code>scala.Stream</code>.
   */
  implicit def StreamMonad[A](as: Stream[A]) = monad[Stream](as)

  /**
   * A monad for <code>scala.Array</code>.
   */
  implicit def ArrayMonad[A](as: Array[A]) = monad[Array](as)

  /**
   * A monad for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Monad[A, B](as: A => B) = monad[PartialType[Function1, A]#Apply](as)

  /**
   * A monad for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherMonad[A, B](as: Either[A, B]) = monad[PartialType[Either, A]#Apply](as)

  /**
   * A monad for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherMonad[A, B](as: Either[B, A]) = monad[PartialType[Either, A]#Flip](as)

  /**
   * A monad for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftMonad[A, B](as: Either.LeftProjection[B, A]) = monad[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A monad for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightMonad[A, B](as: Either.RightProjection[A, B]) = monad[PartialType[Either.RightProjection, A]#Apply](as)

  /**
   * A monad for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateMonad[A, B](as: State[A, B]) = monad[PartialType[State, A]#Apply](as)
}
