package scalaz.control

/**
 * Folding a function across an iterable environment from left to right.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FoldLeft[F[_]] {
  /**
   * Fold the given function across the given environment.
   *
   * @param t The environment to fold across.
   * @param b The beginning value of the fold.
   * @param f The function to fold.
   */
  def foldLeft[B, A](t: F[A], b: B, f: (B, A) => B): B
}

/**
 * Functions over fold-left values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FoldLeft {
  /**
   * A fold-left for identity.
   */
  implicit val IdFoldLeft = new FoldLeft[Tuple1] {
    def foldLeft[B, A](t: Tuple1[A], b: B, f: (B, A) => B) = f(b, t._1)
  }

  /**
   * A fold-left for <code>scala.Option</code>.
   */
  implicit val OptionFoldLeft = new FoldLeft[Option] {
    def foldLeft[B, A](t: Option[A], b: B, f: (B, A) => B) = t match {
      case None => b
      case Some(a) => f(b, a)
    }
  }

  /**
   * A fold-left for <code>scala.List</code>.
   */
  implicit val ListFoldLeft = new FoldLeft[List] {
    def foldLeft[B, A](t: List[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  import list.NonEmptyList

  /**
   * A fold-left for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListFoldLeft = new FoldLeft[NonEmptyList] {
    def foldLeft[B, A](t: NonEmptyList[A], b: B, f: (B, A) => B) = t.toList.foldLeft(b)(f)
  }

  /**
   * A fold-left for <code>scala.Stream</code>.
   */
  implicit val StreamFoldLeft = new FoldLeft[Stream] {
    def foldLeft[B, A](t: Stream[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  /**
   * A fold-left for <code>scala.Array</code>.
   */
  implicit val ArrayFoldLeft = new FoldLeft[Array] {
    def foldLeft[B, A](t: Array[A], b: B, f: (B, A) => B) = t.foldLeft(b)(f)
  }

  /**
   * A fold-left for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherFoldLeft[X] = new FoldLeft[PartialType[Either, X]#Apply] {
    def foldLeft[B, A](t: Either[X, A], b: B, f: (B, A) => B) = t match {
      case Left(_) => b
      case Right(a) => f(b, a)
    }
  }

  /**
   * A fold-left for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherFoldLeft[X] = new FoldLeft[PartialType[Either, X]#Flip] {
    def foldLeft[B, A](t: Either[A, X], b: B, f: (B, A) => B) = t match {
      case Left(a) => f(b, a)
      case Right(_) => b
    }
  }

  /**
   * A fold-left for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftFoldLeft[X] = new FoldLeft[PartialType[Either.LeftProjection, X]#Flip] {
    def foldLeft[B, A](t: Either.LeftProjection[A, X], b: B, f: (B, A) => B) = t.e match {
      case Left(a) => f(b, a)
      case Right(_) => b
    }
  }

  /**
   * A fold-left for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightFoldLeft[X] = new FoldLeft[PartialType[Either.RightProjection, X]#Apply] {
    def foldLeft[B, A](t: Either.RightProjection[X, A], b: B, f: (B, A) => B) = t.e match {
      case Left(_) => b
      case Right(a) => f(b, a)
    }
  }
}

/**
 * Wraps <code>FoldLeft</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see FoldLeft
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait FoldLeftW[F[_], A] {
  /**
   * The fold-left value.
   */
  val v: F[A]

  /**
   * The implementation for the fold-left value.
   */
  val foldleft: FoldLeft[F]

  /**
   * Folds across this iterable.
   */
  final def foldl[B](b: B, f: (B, A) => B) = foldleft.foldLeft(v, b, f)

  /**
   * Folds across this iterable that must contain at least one element (hence, does not require a beginning value). If
   * this iterable is empty, then an error is thrown.
   */
  final def foldl1(f: (A, A) => A) = foldl[Option[A]](None, (a1, a2) => Some(a1 match {
    case None => a2
    case Some(x) => f(a2, x)
  })) getOrElse (error("foldl1 on empty"))

  /**
   * Creates a <code>List</code> from this fold-left. 
   */
  final val list = {
    val b = new scala.collection.mutable.ListBuffer[A]
    foldl[scala.Unit]((), (x, a) => b += a)
    b.toList
  }

  import SemigroupW._
  import Zero._

  /**
   * Sums the contents of this iterable using the given monoid with a left-fold.
   */
  final def suml(implicit m: Monoid[A]) =
    foldl[A](zero[A], _ |+| _)

  /**
   * Returns the number of elements in this iterable.
   */
  final def items = foldl[Int](0, (b, a) => b + 1)

  import Empty._
  import Pure._

  /**
   * Reverses this value using the given monad-empty-plus for reconstruction.
   */
  final def rev[G[_]](implicit m: Empty[G], p: Plus[G], pr: Pure[G]): G[A] =
    foldl[G[A]](empty[G, A], (b, a) => p.plus(pure[G](a), b))

  /**
   * Finds the maximum element in this iterable using the given order. This iterable must not be empty, otherwise an
   * error is thrown.
   */
  final def max(implicit ord: Order[A]) =
    foldl1((x: A, y: A) => if(ord.order(x, y) == GT) x else y)

  /**
   * Finds the minimum element in this iterable using the given order. This iterable must not be empty, otherwise an
   * error is thrown.
   */
  final def min(implicit ord: Order[A]) =
    foldl1((x: A, y: A) => if(ord.order(x, y) == LT) x else y)
}

/**
 * Functions over fold-left values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object FoldLeftW {
  /**
   * Constructs a fold-left from the given value and implementation.
   */
  def foldleft[F[_]] = new PartialWrap[F, FoldLeft, FoldLeftW] {
    def apply[A](fa: F[A])(implicit fl: FoldLeft[F]) = new FoldLeftW[F, A] {
      val v = fa
      val foldleft = fl
    }
  }

  /**
   * A fold-left for identity.
   */
  implicit def IdFoldLeft[A](as: Tuple1[A]) = foldleft[Tuple1](as)

  /**
   * A fold-left for <code>scala.Option</code>.
   */
  implicit def OptionFoldLeft[A](as: Option[A]) = foldleft[Option](as)

  /**
   * A fold-left for <code>scala.List</code>.
   */
  implicit def ListFoldLeft[A](as: List[A]) = foldleft[List](as)

  import list.NonEmptyList

  /**
   * A fold-left for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListFoldLeft[A](as: NonEmptyList[A]) = foldleft[NonEmptyList](as)

  /**
   * A fold-left for <code>scala.Stream</code>.
   */
  implicit def StreamFoldLeft[A](as: Stream[A]) = foldleft[Stream](as)

  /**
   * A fold-left for <code>scala.Array</code>.
   */
  implicit def ArrayFoldLeft[A](as: Array[A]) = foldleft[Array](as)

  /**
   * A fold-left for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherFoldLeft[A, B](as: Either[A, B]) = foldleft[PartialType[Either, A]#Apply](as)

  /**
   * A fold-left for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherFoldLeft[A, B](as: Either[B, A]) = foldleft[PartialType[Either, A]#Flip](as)

  /**
   * A fold-left for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftFoldLeft[A, B](as: Either.LeftProjection[B, A]) = foldleft[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A fold-left for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightFoldLeft[A, B](as: Either.RightProjection[A, B]) = foldleft[PartialType[Either.RightProjection, A]#Apply](as)
}
