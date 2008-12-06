package scalaz.control

/**
 * Folding a function across an iterable environment from right to left.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FoldRight[F[_]] {
  /**
   * Fold the given function across the given environment.
   *
   * @param t The environment to fold across.
   * @param b The beginning value of the fold.
   * @param f The function to fold.
   */
  def foldRight[A, B](t: F[A], b: B, f: (A, => B) => B): B
}

/**
 * Functions over fold-right values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FoldRight {
  /**
   * A fold-right for identity.
   */
  implicit val IdFoldRight = new FoldRight[Tuple1] {
    def foldRight[A, B](t: Tuple1[A], b: B, f: (A, => B) => B) = f(t._1, b)
  }

  /**
   * A fold-right for <code>scala.Option</code>.
   */
  implicit val OptionFoldRight = new FoldRight[Option] {
    def foldRight[A, B](t: Option[A], b: B, f: (A, => B) => B) = t match {
      case None => b
      case Some(a) => f(a, b)
    }
  }

  /**
   * A fold-right for <code>scala.List</code>.
   */
  implicit val ListFoldRight = new FoldRight[List] {
    def foldRight[A, B](t: List[A], b: B, f: (A, => B) => B): B = t match {
      case Nil => b
      case x :: xs => f(x, foldRight(xs, b, f))
    }
  }

  import list.NonEmptyList

  /**
   * A fold-right for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListFoldRight = new FoldRight[NonEmptyList] {
    def foldRight[A, B](t: NonEmptyList[A], b: B, f: (A, => B) => B): B = t match {
      case NonEmptyList(a, Nil) => f(a, b)
      case NonEmptyList(a, as@_ :: _) => f(a, ListFoldRight.foldRight(as, b, f))
    }
  }

  /**
   * A fold-right for <code>scala.Stream</code>.
   */
  implicit val StreamFoldRight = new FoldRight[Stream] {
    def foldRight[A, B](t: Stream[A], b: B, f: (A, => B) => B): B = if(t.isEmpty) b else f(t.head, foldRight(t.tail, b, f))
  }

  /**
   * A fold-right for <code>scala.Array</code>.
   */
  implicit val ArrayFoldRight = new FoldRight[Array] {
    def foldRight[A, B](t: Array[A], b: B, f: (A, => B) => B) = t.foldRight(b)(f(_, _))
  }

  /**
   * A fold-right for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherFoldRight[X] = new FoldRight[PartialType[Either, X]#Apply] {
    def foldRight[A, B](t: Either[X, A], b: B, f: (A, => B) => B) = t match {
      case Left(_) => b
      case Right(a) => f(a, b)
    }
  }

  /**
   * A fold-right for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherFoldRight[X] = new FoldRight[PartialType[Either, X]#Flip] {
    def foldRight[A, B](t: Either[A, X], b: B, f: (A, => B) => B) = t match {
      case Left(a) => f(a, b)
      case Right(_) => b
    }
  }

  /**
   * A fold-right for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftFoldRight[X] = new FoldRight[PartialType[Either.LeftProjection, X]#Flip] {
    def foldRight[A, B](t: Either.LeftProjection[A, X], b: B, f: (A, => B) => B) = t.e match {
      case Left(a) => f(a, b)
      case Right(_) => b
    }
  }

  /**
   * A fold-right for <code>forall T. scala.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightFoldRight[X] = new FoldRight[PartialType[Either.RightProjection, X]#Apply] {
    def foldRight[A, B](t: Either.RightProjection[X, A], b: B, f: (A, => B) => B) = t.e match {
      case Left(_) => b
      case Right(a) => f(a, b)
    }
  }
}

/**
 * Wraps <code>FoldRight</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see FoldRight
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FoldRightW[F[_], A] {
  /**
   * The fold-right value.
   */
  val v: F[A]

  /**
   * The implementation for the fold-right value.
   */
  val foldright: FoldRight[F]

  /**
   * Folds across this iterable.
   */
  final def foldr[B](b: B, f: (A, => B) => B) = foldright.foldRight(v, b, f)

  /**
   * Folds across this iterable that must contain at least one element (hence, does not require a beginning value). If
   * this iterable is empty, then an error is thrown.
   */
  final def foldr1(f: (A, => A) => A) = foldr[Option[A]](None, (a1, a2) => Some(a2 match {
    case None => a1
    case Some(x) => f(a1, x)
  })) getOrElse (error("foldr1 on empty"))

  import SemigroupW._
  import Zero._

  /**
   * Sums the contents of this iterable using the given monoid with a right-fold.
   */
  final def sumr(implicit m: Monoid[A]) =
    foldr[A](zero[A], _ |+| _)

  /**
   * Returns a <code>Stream</code> representation of this iterable.
   */
  final val stream = foldr[Stream[A]](Stream.empty, Stream.cons(_, _))

  /**
   * Returns the element at the given index of this iterable.
   */
  final def !(n: Int) = stream(n)

  /**
   * Returns <code>true</code> if any elements of this iterable satisfy the given predicate, otherwise
   * <code>false</code>.
   */
  final def any(p: A => Boolean) = foldr[Boolean](false, p(_) || _)

  /**
   * Returns <code>true</code> if all of the elements of this iterable satisfy the given predicate, otherwise
   * <code>false</code>.
   */
  final def all(p: A => Boolean) = foldr[Boolean](true, p(_) && _)

  /**
   * Returns <code>true</code> if this iterable is empty, otherwise <code>false</code>.
   */
  final val nil = all(a => false)

  import Empty._
  import Pure._

  /**
   * Intercalates the given separator through this iterable. e.g.
   * <p>
   * <code>[1, 2, 3] intercalate ([7, 8, 9])</code> gives
   * </p>
   * <p>
   * <code>[1, 7, 8, 9, 2, 7, 8, 9, 3]</code>.
   * </p>
   */
  final def intercalate[M[_]](sep: M[A])(implicit e: Empty[M], p: Plus[M], pr: Pure[M]) =
    foldr[(M[A], Boolean)]((empty[M, A], true), (a, b) => (p.plus(p.plus(pure[M](a), if(b._2) empty[M, A] else sep), b._1), false))._1

  /**
   * Returns elements of this iterable that satisfy the given predicate. i.e. a generalised <code>filter</code>.
   */
  final def select[M[_]](f: A => Boolean)(implicit e: Empty[M], p: Plus[M], pr: Pure[M]) =
    foldr[M[A]](empty[M, A], (a, as) => if(f(a)) p.plus(pure[M](a), as) else as)

  /**
   * Returns elements of this iterable while satisfying the given predicate. i.e. a generalised <code>takeWhile</code>.
   */
  final def selectWhile[M[_]](f: A => Boolean)(implicit e: Empty[M], p: Plus[M], pr: Pure[M]) =
    foldr[M[A]](empty[M, A], (a, as) => if(f(a)) p.plus(pure[M](a), as) else empty[M, A])

  /**
   * Splits this iterable into a list of lists, such that each contained list has elements that alternatively
   * satisfiy the predicate.
   * e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] splitWith (_ % 2 == 0)</code> yields
   * </p>
   * <p>
   * <code>[[2, 6, 8], [9], [6], [7, 3, 5], [8, 6], [9]]</code>
   * </p>
   */
  final def splitWith(p: A => Boolean) = foldr[(List[List[A]], Option[Boolean])]((Nil, None), (a, b) => {
      val pa = p(a)
      (b match {
        case (_, None) => List(List(a))
        case (x, Some(q)) => if(pa == q) (a :: x.head) :: x.tail else List(a) :: x
      }, Some(pa))
    })._1

  /**
   * Splits this iterable into a list of lists such that each contained list has elements that satisfy the predicate,
   * with each list split by elements that do not satisfy the predicate. e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] selectSplit (_ % 2 == 0)</code> yields
   * </p>
   * <p>
   * <code>[[2, 6, 8], [6], [8, 6]]</code>
   * </p>
   */
  final def selectSplit(p: A => Boolean) = foldr[(List[List[A]], Boolean)]((Nil, false), (a, xb) => xb match {
      case (x, b) => {
        val pa = p(a)
        (if(pa) if(b) (a :: x.head) :: x.tail else List(a) :: x else x, pa)
      }
    })._1

  import SemigroupW._

  /**
   * Filters through a monadic bind.
   */
  final def filterMonad[M[_], N[_]](f: A => M[Boolean])(implicit m: Monad[M], mo: Monoid[N[A]], p: Pure[N]) =
    foldr[M[N[A]]](m.pure(mo.zero), (a, b) => m.bind((c: Boolean) => m.fmap((ys: N[A]) => if(c) p.pure(a) |+| ys else ys, b), f(a)))

  /**
   * Folds through a monadic bind.
   */
  final def foldrMonad[M[_]] = new {
    def apply[B](b: B, f: (A, B) => M[B])(implicit m: Monad[M]) =
      foldr[M[B]](m.pure(b), (a, x) => m.bind(f(a, (_: B)), x))
  }
}

/**
 * Functions over fold-right values.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FoldRightW {
  /**
   * Constructs a fold-right from the given value and implementation.
   */
  def foldright[F[_]] = new PartialWrap[F, FoldRight, FoldRightW] {
    def apply[A](fa: F[A])(implicit fr: FoldRight[F]) = new FoldRightW[F, A] {
      val v = fa
      val foldright = fr
    }
  }

  /**
   * A fold-right for identity.
   */
  implicit def IdFoldRight[A](as: Tuple1[A]) = foldright[Tuple1](as)

  /**
   * A fold-right for <code>scala.Option</code>.
   */
  implicit def OptionFoldRight[A](as: Option[A]) = foldright[Option](as)

  /**
   * A fold-right for <code>scala.List</code>.
   */
  implicit def ListFoldRight[A](as: List[A]) = foldright[List](as)

  import list.NonEmptyList

  /**
   * A fold-right for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListFoldRight[A](as: NonEmptyList[A]) = foldright[NonEmptyList](as)

  /**
   * A fold-right for <code>scala.Stream</code>.
   */
  implicit def StreamFoldRight[A](as: Stream[A]): FoldRightW[Stream, A] = foldright[Stream](as)

  /**
   * A fold-right for <code>scala.Array</code>.
   */
  implicit def ArrayFoldRight[A](as: Array[A]) = foldright[Array](as)

  /**
   * A fold-right for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherFoldRight[A, B](as: Either[A, B]) = foldright[PartialType[Either, A]#Apply](as)

  /**
   * A fold-right for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherFoldRight[A, B](as: Either[B, A]) = foldright[PartialType[Either, A]#Flip](as)

  /**
   * A fold-right for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftFoldRight[A, B](as: Either.LeftProjection[B, A]) = foldright[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A fold-right for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightFoldRight[A, B](as: Either.RightProjection[A, B]) = foldright[PartialType[Either.RightProjection, A]#Apply](as)
}
