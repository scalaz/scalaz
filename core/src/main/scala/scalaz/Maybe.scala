package scalaz

import scala.annotation.tailrec
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

  final def flatMap[B](f: A => Maybe[B]): Maybe[B] =
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
  final def orEmpty[F[_]](implicit F: ApplicativePlus[F]): F[A] =
    cata(F.point(_), F.empty)

  final def orError[F[_], E](e: E)(implicit F: MonadError[F, E]): F[A] =
    cata(F.point(_), F.raiseError(e))

}

object Maybe extends MaybeInstances {

  sealed abstract case class Empty[A] private() extends Maybe[A]
  object Empty {
    // #1712: covariant subclass of `INil` makes the pattern matcher see it as covariant
    private[this] final class _Empty[+A] extends Empty[A]
    private[this] val value = new _Empty[Nothing]
    def apply[A](): Maybe[A] = value.asInstanceOf[Empty[A]]
  }

  // `get` is an intentional name as it is expected by the unapply
  // logic in the scalac pattern matcher.
  final case class Just[A](get: A) extends Maybe[A]

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

  @deprecated("Throwable is not referentially transparent, use \\/.attempt", "7.3.0")
  def fromTryCatchThrowable[T, E <: Throwable: NotNothing](a: => T)(implicit ex: ClassTag[E]): Maybe[T] = try {
    just(a)
  } catch {
    case e if ex.runtimeClass.isInstance(e) => empty
  }

  @deprecated("Throwable is not referentially transparent, use \\/.attempt", "7.3.0")
  def fromTryCatchNonFatal[T](a: => T): Maybe[T] = try {
    just(a)
  } catch {
    case NonFatal(t) => empty
  }

  /**
   * For interfacing with legacy, deterministic, partial functions. See
   * [[\/.attempt]] for further details.
   */
  def attempt[T](a: => T): Maybe[T] = try {
    just(a)
  } catch {
    case NonFatal(_) => empty
  }

}

sealed abstract class MaybeInstances1 {
  implicit def maybeBand[A: Band]: Band[Maybe[A]] =
    new MaybeMonoid[A] with Band[Maybe[A]] {
      override def A = implicitly
    }
}

sealed abstract class MaybeInstances0 extends MaybeInstances1 {
  implicit def maybeSemiLattice[A](implicit A: SemiLattice[A]): SemiLattice[Maybe[A]] =
    new SemiLattice[Maybe[A]] with Band[Maybe[A]] {
      override def append(fa1: Maybe[A], fa2: => Maybe[A]) =
        fa1.cata(
          a1 => fa2.cata(a2 => Maybe.just(A.append(a1, a2)), fa1),
          fa2.cata(_ => fa2, Maybe.empty))
    }
}

sealed abstract class MaybeInstances extends MaybeInstances0 {
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

  implicit def maybeShow[A](implicit A: Show[A]): Show[Maybe[A]] = {
    import scalaz.syntax.show._
    Show.show(_.cata(
      a => cord"Just($a)",
      Cord("Empty")))
  }

  implicit def maybeMonoid[A: Semigroup]: Monoid[Maybe[A]] =
    new MaybeMonoid[A] {
      override def A = implicitly
    }

  implicit def maybeFirstMonoid[A]: Monoid[FirstMaybe[A]] with Band[FirstMaybe[A]] = new Monoid[FirstMaybe[A]] with Band[FirstMaybe[A]] {
    val zero: FirstMaybe[A] = Tag(empty)

    def append(fa1: FirstMaybe[A], fa2: => FirstMaybe[A]): FirstMaybe[A] = Tag(Tag.unwrap(fa1).orElse(Tag.unwrap(fa2)))
  }

  implicit def maybeFirstShow[A](implicit A: Show[Maybe[A]]): Show[FirstMaybe[A]] = Tag.subst(A)

  implicit def maybeFirstOrder[A](implicit A: Order[Maybe[A]]): Order[FirstMaybe[A]] = Tag.subst(A)

  implicit def maybeFirstMonad: Monad[FirstMaybe] = Tags.First.subst1[Monad, Maybe](Monad[Maybe])

  implicit def maybeLastMonoid[A]: Monoid[LastMaybe[A]] with Band[LastMaybe[A]] = new Monoid[LastMaybe[A]] with Band[LastMaybe[A]] {
    val zero: LastMaybe[A] = Tag(empty)

    def append(fa1: LastMaybe[A], fa2: => LastMaybe[A]): LastMaybe[A] = Tag(Tag.unwrap(fa2).orElse(Tag.unwrap(fa1)))
  }

  implicit def maybeLastShow[A](implicit A: Show[Maybe[A]]): Show[LastMaybe[A]] = Tag.subst(A)

  implicit def maybeLastOrder[A](implicit A: Order[Maybe[A]]): Order[LastMaybe[A]] = Tag.subst(A)

  implicit def maybeLastMonad: Monad[LastMaybe] = Tags.Last.subst1[Monad, Maybe](Monad[Maybe])

  implicit def maybeMin[A](implicit o: Order[A]): Monoid[MinMaybe[A]] with Band[MinMaybe[A]] = new Monoid[MinMaybe[A]] with Band[MinMaybe[A]] {
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

  implicit def maybeMax[A](implicit o: Order[A]): Monoid[MaxMaybe[A]] with Band[MaxMaybe[A]] = new Monoid[MaxMaybe[A]] with Band[MaxMaybe[A]] {
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

  implicit val maybeInstance: Traverse[Maybe] with MonadPlus[Maybe] with Alt[Maybe] with BindRec[Maybe] with Cozip[Maybe] with Zip[Maybe] with Unzip[Maybe] with Align[Maybe] with IsEmpty[Maybe] with Cobind[Maybe] with Optional[Maybe] =
    new Traverse[Maybe] with MonadPlus[Maybe] with Alt[Maybe] with BindRec[Maybe] with Cozip[Maybe] with Zip[Maybe] with Unzip[Maybe] with Align[Maybe] with IsEmpty[Maybe] with Cobind[Maybe] with Optional[Maybe] {

      def point[A](a: => A) = just(a)

      override def ap[A, B](fa: => Maybe[A])(mf: => Maybe[A => B]) =
        mf.cata(f => fa.cata(f andThen just, empty), empty)

      def bind[A, B](fa: Maybe[A])(f: A => Maybe[B]) = fa flatMap f

      @tailrec def tailrecM[A, B](a: A)(f: A => Maybe[A \/ B]): Maybe[B] =
        f(a) match {
          case Empty() => Empty()
          case Just(-\/(a)) => tailrecM(a)(f)
          case Just(\/-(b)) => Just(b)
        }

      override def map[A, B](fa: Maybe[A])(f: A => B) = fa map f

      def traverseImpl[F[_], A, B](fa: Maybe[A])(f: A => F[B])(implicit F: Applicative[F]) =
        fa.cata(a => F.map(f(a))(just), F.point(empty))

      def empty[A]: Maybe[A] = Maybe.empty

      def plus[A](a: Maybe[A], b: => Maybe[A]) = a orElse b

      override def unfoldrPsumOpt[S, A](seed: S)(f: S => Maybe[(Maybe[A], S)]): Maybe[Maybe[A]] = {
        @tailrec def go(s: S): Maybe[A] = f(s) match {
          case Just((ma, s)) => ma match {
            case a @ Just(_) => a
            case _ => go(s)
          }
          case Empty() => Empty()
        }
        f(seed) map { case (ma, s) => ma match {
          case a @ Just(_) => a
          case Empty() => go(s)
        }}
      }

      override def unfoldrOpt[S, A, B](seed: S)(f: S => Maybe[(Maybe[A], S)])(implicit r: Reducer[A, B]): Maybe[Maybe[B]] = {
        @tailrec def go(acc: B, s: S): Maybe[B] = f(s) match {
          case Just((ma, s)) => ma match {
            case Just(a) => go(r.snoc(acc, a), s)
            case _ => Empty()
          }
          case _ => Just(acc)
        }
        f(seed) map { case (ma, s) => ma match {
          case Just(a) => go(r.unit(a), s)
          case _ => Empty()
        }}
      }

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

      override def alt[A](a1: => Maybe[A], a2: => Maybe[A]): Maybe[A] = a1 orElse a2

      // performance optimisation
      override def altly2[Z, A1, A2](a1: => Maybe[A1], a2: => Maybe[A2])(f: A1 \/ A2 => Z): Maybe[Z] = a1 match {
          case Empty() => a2 match {
            case Empty() => Empty()
            case j => j.map(s => f(\/-(s)))
          }
          case j => j.map(s => f(-\/(s)))
        }

      // performance optimisation
      override def apply2[A, B, C](fa: => Maybe[A], fb: => Maybe[B])(f: (A, B) => C): Maybe[C] =
        fa.flatMap(a => fb.map(b => f(a, b)))
    }
}

private sealed trait MaybeEqual[A] extends Equal[Maybe[A]] {
  implicit def A: Equal[A]

  override final def equal(fa1: Maybe[A], fa2: Maybe[A]) =
    fa1.cata(
      a1 => fa2.cata(a2 => A.equal(a1, a2), false),
      fa2.cata(_ => false, true))
}

private sealed trait MaybeMonoid[A] extends Monoid[Maybe[A]] {
  protected def A: Semigroup[A]

  override def append(fa1: Maybe[A], fa2: => Maybe[A]) =
    fa1.cata(
      a1 => fa2.cata(a2 => Maybe.just(A.append(a1, a2)), fa1),
      fa2.cata(_ => fa2, Maybe.empty))

  override def zero = Maybe.empty
}
