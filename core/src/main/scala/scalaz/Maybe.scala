package scalaz

import scala.util.control.NonFatal
import scala.reflect.ClassTag
import Ordering._
import Isomorphism.{<~>, IsoFunctorTemplate}

/** An optional value
 *
 * A `Maybe[A]` will either be a wrapped `A` instance (`Just[A]`), or a lack of underlying
 * `A` instance (`Empty[A]`).
 *
 * `Maybe[A]` is isomorphic to `Option[A]`, however there are some differences between
 * the two. `Maybe` is invariant in `A` while `Option` is covariant. `Maybe[A]` does not expose
 * an unsafe `get` operation to access the underlying `A` value (that may not exist) like
 * `Option[A]` does. `Maybe[A]` does not come with an implicit conversion to `Iterable[A]` (a
 * trait with over a dozen super types).
 */
sealed abstract class Maybe[A] {
  import Maybe._

  /** Catamorphism.
   * Run the given function on the underlying value if present, otherwise return
   * the provided fallback value */
  final def cata[B](f: A => B, b: => B): B =
    this match {
      case Just(a) => f(a)
      case Empty() => b
    }

  /** Return the underlying value if present, otherwise the provided fallback value */
  final def getOrElse(a: => A): A =
    cata(identity, a)

  /** alias for [[getOrElse]] */
  final def |(a: => A): A =
    getOrElse(a)

  /** Turn the underlying value into a failure validation if present, otherwise
   * return a success validation with the provided fallback value */
  final def toFailure[B](b: => B): Validation[A, B] =
    cata(Validation.failure, Success(b))

  /** Turn the underlying value into a success validation if present, otherwise
   * return a failure validation with the provided fallback value */
  final def toSuccess[B](b: => B): Validation[B, A] =
    cata(Validation.success, Failure(b))

  /** Turn the underlying value into a left disjunction if present, otherwise
   * return a right disjunction with the provided fallback value */
  final def toLeft[B](b: => B): A \/ B =
    cata(\/.left, \/-(b))

  /** alias for [[toLeft]] */
  final def <\/[B](b: => B): A \/ B =
    toLeft(b)

  /** Turn the underlying value into a right disjunction if present, otherwise
   * return a left disjunction with the provided fallback value */
  final def toRight[B](b: => B): B \/ A =
    cata(\/.right, -\/(b))

  /** alias for [[toRight]] */
  final def \/>[B](b: => B): B \/ A =
    toRight(b)

  /** True if an underlying value is present */
  final def isJust: Boolean =
    cata(_ => true, false)

  /** True if no underlying value is present */
  final def isEmpty: Boolean =
    cata(_ => false, true)

  final def map[B](f: A => B): Maybe[B] =
    cata(f andThen just[B], empty[B])

  final def flatMap[B](f: A => Maybe[B]) =
    cata(f, empty[B])

  /** Convert to a standard library `Option` */
  final def toOption: Option[A] =
    cata(Some(_), None)

  /** Return this instance if it is a [[Maybe.Just]], otherwise the provided fallback */
  final def orElse(oa: => Maybe[A]): Maybe[A] =
    cata(_ => this, oa)

  final def getOrElseF[F[_]: Applicative](fa: => F[A]): F[A] =
    cata(Applicative[F].point[A](_), fa)

  /** Tag with [[Tags.First]] */
  final def first: FirstMaybe[A] = Tag(this)

  /** Tag with [[Tags.Last]] */
  final def last: LastMaybe[A] = Tag(this)

  /** Tag with [[Tags.Min]] */
  final def min: MinMaybe[A] = Tag(this)

  /** Tag with [[Tags.Max]] */
  final def max: MaxMaybe[A] = Tag(this)

  final def cojoin: Maybe[Maybe[A]] = map(just)

  final def cobind[B](f: Maybe[A] => B): Maybe[B] =
    map(_ => f(this))

  final def zip[B](fb: Maybe[B]): Maybe[(A, B)] =
    for {
      a <- this
      b <- fb
    } yield (a, b)

  final def zipWith[B, C](fb: Maybe[B])(f: (A, B) => C): Maybe[C] =
    for {
      a <- this
      b <- fb
    } yield f(a, b)

  final def filter(f: A => Boolean): Maybe[A] =
    flatMap(a => if (f(a)) this else empty)

  final def filterNot(f: A => Boolean): Maybe[A] =
    filter(f.andThen(!_))

  /** Return `true` if this is a [[Maybe.Empty]] or if this is a [[Maybe.Just]]
   * and the underlying value satisfies the provided predicate */
  final def forall(f: A => Boolean): Boolean =
    cata(f, true)

  /** Return `true` if this is a [[Maybe.Just]] and the underlying value
   * satisfies the provided predicate */
  final def exists(f: A => Boolean): Boolean =
    cata(f, false)

  /** Return the underlying value if present, otherwise the monoid zero */
  final def orZero(implicit F: Monoid[A]): A =
    getOrElse(F.zero)

  /** alias for [[orZero]] */
  final def unary_~(implicit z: Monoid[A]): A =
    orZero

  /**
   * Return the underlying value wrapped in type `F` if present, otherwise the
   * empty value for type `F` */
  final def orEmpty[F[_]](implicit F: Applicative[F], G: PlusEmpty[F]): F[A] =
    cata(F.point(_), G.empty)
}

object Maybe extends MaybeInstances {

  final case class Empty[A]() extends Maybe[A]

  final case class Just[A](a: A) extends Maybe[A]

  val optionMaybeIso: Option <~> Maybe =
    new IsoFunctorTemplate[Option, Maybe] {
      def to[A](fa: Option[A]) = std.option.toMaybe(fa)
      def from[A](ga: Maybe[A]) = ga.toOption
    }

  /** Wrap a value in Just, or return Empty if the value is null */
  final def fromNullable[A](a: A): Maybe[A] =
    if (null == a) empty else just(a)

  final def empty[A]: Maybe[A] = Empty()

  final def just[A](a: A): Maybe[A] = Just(a)

  final def fromOption[A](oa: Option[A]): Maybe[A] =
    std.option.cata(oa)(just, empty)

  def fromTryCatchThrowable[T, E <: Throwable](a: => T)(implicit nn: NotNothing[E], ex: ClassTag[E]): Maybe[T] = try {
    just(a)
  } catch {
    case e if ex.runtimeClass.isInstance(e) => empty
  }

  def fromTryCatchNonFatal[T](a: => T): Maybe[T] = try {
    just(a)
  } catch {
    case NonFatal(t) => empty
  }
}

sealed abstract class MaybeInstances {
  import Maybe._

  implicit def maybeEqual[A : Equal]: Equal[Maybe[A]] = new MaybeEqual[A] {
    def A = implicitly
  }

  implicit def maybeOrder[A : Order]: Order[Maybe[A]] = new Order[Maybe[A]] with MaybeEqual[A] {
    def A = implicitly

    def order(fa1: Maybe[A], fa2: Maybe[A]) =
      fa1.cata(
        a1 => fa2.cata(
          a2 => Order[A].order(a1, a2),
          GT),
        fa2.cata(_ => LT, EQ))
  }

  implicit def maybeShow[A](implicit A: Show[A]): Show[Maybe[A]] =
    Show.show(_.cata(
      a => Cord("Just(", A.show(a), ")"),
      "Empty"))

  implicit def maybeMonoid[A](implicit A: Semigroup[A]): Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    def append(fa1: Maybe[A], fa2: => Maybe[A]) =
      fa1.cata(
        a1 => fa2.cata(a2 => just(A.append(a1, a2)), fa1),
        fa2.cata(_ => fa2, empty))

    def zero = empty
  }

  implicit def maybeFirstMonoid[A]: Monoid[FirstMaybe[A]] = new Monoid[FirstMaybe[A]] {
    val zero: FirstMaybe[A] = Tag(empty)

    def append(fa1: FirstMaybe[A], fa2: => FirstMaybe[A]): FirstMaybe[A] = Tag(Tag.unwrap(fa1).orElse(Tag.unwrap(fa2)))
  }

  implicit def maybeFirstShow[A](implicit A: Show[Maybe[A]]): Show[FirstMaybe[A]] = Tag.subst(A)

  implicit def maybeFirstOrder[A](implicit A: Order[Maybe[A]]): Order[FirstMaybe[A]] = Tag.subst(A)

  implicit def maybeFirstMonad: Monad[FirstMaybe] = Tags.First.subst1[Monad, Maybe](Monad[Maybe])

  implicit def maybeLastMonoid[A]: Monoid[LastMaybe[A]] = new Monoid[LastMaybe[A]] {
    val zero: LastMaybe[A] = Tag(empty)

    def append(fa1: LastMaybe[A], fa2: => LastMaybe[A]): LastMaybe[A] = Tag(Tag.unwrap(fa2).orElse(Tag.unwrap(fa1)))
  }

  implicit def maybeLastShow[A](implicit A: Show[Maybe[A]]): Show[LastMaybe[A]] = Tag.subst(A)

  implicit def maybeLastOrder[A](implicit A: Order[Maybe[A]]): Order[LastMaybe[A]] = Tag.subst(A)

  implicit def maybeLastMonad: Monad[LastMaybe] = Tags.Last.subst1[Monad, Maybe](Monad[Maybe])

  implicit def maybeMin[A](implicit o: Order[A]) = new Monoid[MinMaybe[A]] {
    def zero: MinMaybe[A] = Tag(empty)

    def append(f1: MinMaybe[A], f2: => MinMaybe[A]) = Tag( (Tag unwrap f1, Tag unwrap f2) match {
      case (Just(v1), Just(v2)) => Just(Order[A].min(v1, v2))
      case (_f1 @ Just(_), Empty()) => _f1
      case (Empty(), _f2 @ Just(_)) => _f2
      case (Empty(), Empty()) => empty
    })
  }

  implicit def maybeMinShow[A: Show]: Show[MinMaybe[A]] = Tag.subst(Show[Maybe[A]])

  implicit def maybeMinOrder[A: Order]: Order[MinMaybe[A]] = Tag.subst(Order[Maybe[A]])

  implicit def maybeMinMonad: Monad[MinMaybe] = Tags.Min.subst1[Monad, Maybe](Monad[Maybe])

  implicit def maybeMax[A](implicit o: Order[A]) = new Monoid[MaxMaybe[A]] {
    def zero: MaxMaybe[A] = Tag(empty)

    def append(f1: MaxMaybe[A], f2: => MaxMaybe[A]) = Tag( (Tag unwrap f1, Tag unwrap f2) match {
      case (Just(v1), Just(v2)) => Just(Order[A].max(v1, v2))
      case (_f1 @ Just(_), Empty()) => _f1
      case (Empty(), _f2 @ Just(_)) => _f2
      case (Empty(), Empty()) => Empty()
    })
  }

  implicit def maybeMaxShow[A: Show]: Show[MaxMaybe[A]] = Tag.subst(Show[Maybe[A]])

  implicit def maybeMaxOrder[A: Order]: Order[MaxMaybe[A]] = Tag.subst(Order[Maybe[A]])

  implicit def maybeMaxMonad: Monad[MaxMaybe] = Tags.Max.subst1[Monad, Maybe](Monad[Maybe])

  implicit val maybeInstance: Traverse[Maybe] with MonadPlus[Maybe] with BindRec[Maybe] with Cozip[Maybe] with Zip[Maybe] with Unzip[Maybe] with Align[Maybe] with IsEmpty[Maybe] with Cobind[Maybe] with Optional[Maybe] =
    new Traverse[Maybe] with MonadPlus[Maybe] with BindRec[Maybe] with Cozip[Maybe] with Zip[Maybe] with Unzip[Maybe] with Align[Maybe] with IsEmpty[Maybe] with Cobind[Maybe] with Optional[Maybe] {

      def point[A](a: => A) = just(a)

      override def ap[A, B](fa: => Maybe[A])(mf: => Maybe[A => B]) =
        mf.cata(f => fa.cata(f andThen just, empty), empty)

      def bind[A, B](fa: Maybe[A])(f: A => Maybe[B]) = fa flatMap f

      @scala.annotation.tailrec
      def tailrecM[A, B](f: A => Maybe[A \/ B])(a: A): Maybe[B] =
        f(a) match {
          case Empty() => Empty()
          case Just(-\/(a)) => tailrecM(f)(a)
          case Just(\/-(b)) => Just(b)
        }

      override def map[A, B](fa: Maybe[A])(f: A => B) = fa map f

      def traverseImpl[F[_], A, B](fa: Maybe[A])(f: A => F[B])(implicit F: Applicative[F]) =
        fa.cata(a => F.map(f(a))(just), F.point(empty))

      def empty[A]: Maybe[A] = Maybe.empty

      def plus[A](a: Maybe[A], b: => Maybe[A]) = a orElse b

      override def foldRight[A, B](fa: Maybe[A], z: => B)(f: (A, => B) => B) =
        fa.cata(f(_, z), z)

      def cozip[A, B](fa: Maybe[A \/ B]) =
        fa.cata(_.leftMap(just).map(just), -\/(empty))

      def zip[A, B](a: => Maybe[A], b: => Maybe[B]) = a.zip(b)

      def unzip[A, B](a: Maybe[(A, B)]) =
        a.cata(ab => (just(ab._1), just(ab._2)), (empty, empty))

      def alignWith[A, B, C](f: A \&/ B => C) = (fa, fb) =>
        fa.cata(
          a => fb.cata(
            b => just(f(\&/.Both(a, b))),
            just(f(\&/.This(a)))),
          fb.cata(
            b => just(f(\&/.That(b))),
            empty))

      def cobind[A, B](fa: Maybe[A])(f: Maybe[A] => B) =
        fa.cobind(f)

      override def cojoin[A](a: Maybe[A]) =
        a.cojoin

      def pextract[B, A](fa: Maybe[A]): Maybe[B] \/ A =
        fa.cata(\/.right, -\/(empty))

      override def isDefined[A](fa: Maybe[A]): Boolean = fa.isJust

      override def toOption[A](fa: Maybe[A]): Option[A] = fa.toOption

      override def toMaybe[A](fa: Maybe[A]) = fa

      override def filter[A](fa: Maybe[A])(f: A => Boolean): Maybe[A] =
        fa.filter(f)
    }
}

private sealed trait MaybeEqual[A] extends Equal[Maybe[A]] {
  implicit def A: Equal[A]

  override final def equal(fa1: Maybe[A], fa2: Maybe[A]) =
    fa1.cata(
      a1 => fa2.cata(a2 => A.equal(a1, a2), false),
      fa2.cata(_ => false, true))
}
