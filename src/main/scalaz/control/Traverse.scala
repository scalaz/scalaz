package scalaz.control

/**
 * Defines a traversable type as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>. Also see
 * <a href="http://citeseer.ist.psu.edu/536643.html">The Essence of the Iterator Pattern</a> by Saad Alfoudari and
 * L.J. Steggles for further work.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Traverse[T[_]] {
  /**
   * Maps each element of the environment onto an action, evaluating from left to right and collecting the results.
   *
   * @param f The function to map on each element in the environment.
   * @param ta The collection of elements to map.
   */
  def traverse[F[_], A, B](f: A => F[B], ta: T[A])(implicit a: Applicative[F]): F[T[B]]
}

/**
 * Functions over traversable values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Traverse {
  import ApplicativeW._
  import Pure._

  /**
   * A traversable for identity.
   */
  implicit val IdTraverse: Traverse[Tuple1] = new Traverse[Tuple1] {
    def traverse[F[_], A, B](f: A => F[B], ta: Tuple1[A])(implicit a: Applicative[F]) =
      applicative[F](f(ta._1)) > (Tuple1(_: B))
  }

  /**
   * A traversable for <code>scala.Option</code>.
   */
  implicit val OptionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[F[_], A, B](f: A => F[B], ta: Option[A])(implicit a: Applicative[F]): F[Option[B]] =
      ta match {
        case None => pure[F](None)
        case Some(x) => applicative[F](f(x)) > (Some(_: B))
      }
  }

  /**
   * A traversable for <code>scala.List</code>.
   */
  implicit val ListTraverse: Traverse[List] = new Traverse[List] {
    def traverse[F[_], A, B](f: A => F[B], as: List[A])(implicit a: Applicative[F]): F[List[B]] =
      as.foldRight[F[List[B]]](pure[F](Nil))((x, ys) => a(applicative[F](f(x)) > ((a: B) => (b: List[B]) => a :: b), ys))
  }

  import FoldRightW._

  /**
   * A traversable for <code>scala.Stream</code>.
   */
  implicit val StreamTraverse: Traverse[Stream] = new Traverse[Stream] {
    def traverse[F[_], A, B](f: A => F[B], as: Stream[A])(implicit a: Applicative[F]): F[Stream[B]] =
      as.foldr[F[Stream[B]]](pure[F](Stream.empty), (x, ys) =>
        a(applicative[F](f(x)) > ((a: B) => (b: Stream[B]) => Stream.cons(a, b)), ys))
  }

  /**
   * A traversable for <code>scala.Array</code>.
   */  implicit val ArrayTraverse: Traverse[Array] = new Traverse[Array] {
    def traverse[F[_], A, B](f: A => F[B], as: Array[A])(implicit a: Applicative[F]): F[Array[B]] =
      applicative[F](ListTraverse.traverse[F, A, B](f, as.toList)) > ((_: List[B]).toArray)
  }

  /**
   * A traversable for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherTraverse[X]: Traverse[PartialType[Either, X]#Apply] = new Traverse[PartialType[Either, X]#Apply] {
    def traverse[F[_], A, B](f: A => F[B], as: Either[X, A])(implicit a: Applicative[F]): F[Either[X, B]] =
      as match {
        case Left(x) => pure[F](Left(x))
        case Right(x) => applicative[F](f(x)) > (Right(_: B))
      }
  }

  /**
   * A traversable for <code>forall T. scala.Either[?, T]</code>.
   */  implicit def FlipEitherTraverse[X]: Traverse[PartialType[Either, X]#Flip] = new Traverse[PartialType[Either, X]#Flip] {
    def traverse[F[_], A, B](f: A => F[B], as: Either[A, X])(implicit a: Applicative[F]): F[Either[B, X]] =
      as match {
        case Right(x) => pure[F](Right(x))
        case Left(x) => applicative[F](f(x)) > (Left(_: B))
      }
  }

  /**
   * A traversable for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftTraverse[X]: Traverse[PartialType[Either.LeftProjection, X]#Flip] = new Traverse[PartialType[Either.LeftProjection, X]#Flip] {
    def traverse[F[_], A, B](f: A => F[B], as: Either.LeftProjection[A, X])(implicit a: Applicative[F]): F[Either.LeftProjection[B, X]] =
      as.e match {
        case Right(x) => pure[F](Right(x).left)
        case Left(x) => applicative[F](f(x)) > (Left(_: B).left)
      }
  }

  /**
   * A traversable for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightTraverse[X]: Traverse[PartialType[Either.RightProjection, X]#Apply] = new Traverse[PartialType[Either.RightProjection, X]#Apply] {
    def traverse[F[_], A, B](f: A => F[B], as: Either.RightProjection[X, A])(implicit a: Applicative[F]): F[Either.RightProjection[X, B]] =
      as.e match {
        case Left(x) => pure[F](Left(x).right)
        case Right(x) => applicative[F](f(x)) > (Right(_: B).right)
      }
  }

  import validation.Validation

  /**
   * A traversable for <code>scalaz.validation.Validation</code>.
   */
  implicit def ValidationTraverse[X]: Traverse[PartialType[Validation, X]#Apply] = new Traverse[PartialType[Validation, X]#Apply] {
    def traverse[F[_], A, B](f: A => F[B], as: Validation[X, A])(implicit a: Applicative[F]): F[Validation[X, B]] =
      applicative[F](EitherTraverse.traverse[F, A, B](f, as.either)) > ((t: Either[X, B]) => (t: Validation[X, B]))
  }

  /**
   * Traverse on the identity function.
   */
  def sequence[F[_], T[_]] = new {
    def apply[A](x: T[F[A]])(implicit a: Applicative[F], t: Traverse[T]): F[T[A]] = t.traverse[F, F[A], A](identity, x)
  }
}

/**
 * Wraps <code>Traversable</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Traversable
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait TraverseW[T[_], A] extends FunctorW[T, A] {
  /**
   * The traversable value.
   */
  val v: T[A]

  /**
   * The traversable implementation for the value.
   */
  val traversable: Traverse[T]

  /**
   * Maps each element of this traversable onto an action, evaluating from left to right and collecting the results.
   *
   * @param f The function to map on each element in the environment.
   */
  final def traverse[F[_]] = new {
    def apply[B](f: A => F[B])(implicit a: Applicative[F]) = traversable.traverse[F, A, B](f, v)
  }

  import SemigroupW._
  import Zero._

  /**
   * Accumulates using the given monoidal append.
   */
  final def -->>[B](f: A => B)(implicit m: Monoid[B]): B = {
    case class Acc[B, A](acc: B)

    implicit val AccApplicative = new Applicative[PartialType[Acc, B]#Apply] {
      def apply[A, X](f: Acc[B, A => X], fa: Acc[B, A]) = Acc[B, X](f.acc |+| fa.acc)
      def pure[A](a: A) = Acc[B, A](zero[B])
    }

    traverse[PartialType[Acc, B]#Apply](a => Acc[B, B](f(a))).acc
  }

  /**
   * Accumulates with identity.
   */
  final def ->>(implicit m: Monoid[A]) = -->>(identity[A])
}

/**
 * Functions over traversable values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object TraverseW {
  /**
   * Constructs a traversable from the given value and implementation.
   */
  def traverse[T[_]] = new PartialWrap[T, Traverse, TraverseW] {
    def apply[A](a: T[A])(implicit t: Traverse[T]) = new TraverseW[T, A] {
      val v = a

      val traversable = t

      val functor = new Functor[T] {
        def fmap[A, B](f: A => B, fa: T[A]) = t.traverse[Tuple1, A, B](g => Tuple1(f(g)), fa)._1
      }
    }
  }

  /**
   * A traversable for identity.
   */
  implicit def IdTraverse[A](as: Tuple1[A]) = traverse[Tuple1](as)

  /**
   * A traversable for <code>scala.Option</code>.
   */
  implicit def OptionTraverse[A](as: Option[A]) = traverse[Option](as)

  /**
   * A traversable for <code>scala.List</code>.
   */
  implicit def ListTraverse[A](as: List[A]) = traverse[List](as)

  /**
   * A traversable for <code>scala.Stream</code>.
   */
  implicit def StreamTraverse[A](as: Stream[A]) = traverse[Stream](as)

  /**
   * A traversable for <code>scala.Array</code>.
   */
  implicit def ArrayTraverse[A](as: Array[A]) = traverse[Array](as)

  /**
   * A traversable for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherTraverse[A, B](as: Either[A, B]) = traverse[PartialType[Either, A]#Apply](as)

  /**
   * A traversable for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherTraverse[A, B](as: Either[B, A]) = traverse[PartialType[Either, A]#Flip](as)

  /**
   * A traversable for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftTraverse[A, B](as: Either.LeftProjection[B, A]) = traverse[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * A traversable for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightTraverse[A, B](as: Either.RightProjection[A, B]) = traverse[PartialType[Either.RightProjection, A]#Apply](as)

  import validation.Validation

  /**
   * A traversable for <code>forall T. scalaz.validation.Validation[T, ?]</code>.
   */
  implicit def ValidationTraverse[A, B](as: Validation[A, B]) = traverse[PartialType[Validation, A]#Apply](as)
}
