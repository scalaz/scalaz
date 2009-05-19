package scalaz

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

object Apply {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }

  import Scalaz._

  implicit val IdentityApply: Apply[Identity] = FunctorBindApply[Identity]

  implicit def ContinuationApply[R] = FunctorBindApply[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListApply = FunctorBindApply[NonEmptyList]

  implicit def StateApply[S] = FunctorBindApply[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Apply = FunctorBindApply[Tuple1]

  implicit def Tuple2Apply[R](implicit sr: Semigroup[R]) = FunctorBindApply[PartialApply1Of2[Tuple2, R]#Apply]

  implicit def Tuple3Apply[R, S](implicit sr: Semigroup[R], ss: Semigroup[S]) = FunctorBindApply[PartialApply2Of3[Tuple3, R, S]#Apply]

  implicit def Tuple4Apply[R, S, T](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T]) = FunctorBindApply[PartialApply3Of4[Tuple4, R, S, T]#Apply]

  implicit def Tuple5Apply[R, S, T, U](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U]) = FunctorBindApply[PartialApply4Of5[Tuple5, R, S, T, U]#Apply]

  implicit def Tuple6Apply[R, S, T, U, V](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U], sv: Semigroup[V]) = FunctorBindApply[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply]

  implicit def Tuple7Apply[R, S, T, U, V, W](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U], sv: Semigroup[V], sw: Semigroup[W]) = FunctorBindApply[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply]

  implicit val Function0Apply = FunctorBindApply[Function0]

  implicit def Function1Apply[R] = FunctorBindApply[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Apply[R, S] = FunctorBindApply[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Apply[R, S, T] = FunctorBindApply[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Apply[R, S, T, U] = FunctorBindApply[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Apply[R, S, T, U, V] = FunctorBindApply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Apply[R, S, T, U, V, W] = FunctorBindApply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListApply = FunctorBindApply[List]

  implicit val StreamApply = FunctorBindApply[Stream]

  implicit val OptionApply = FunctorBindApply[Option]

  implicit val ArrayApply = FunctorBindApply[Array]

  implicit def EitherLeftApply[X] = FunctorBindApply[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightApply[X] = FunctorBindApply[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit def ValidationApply[X](implicit s: Semigroup[X]) = new Apply[PartialApply1Of2[Validation, X]#Apply] {
    def apply[A, B](f: Validation[X, A => B], a: Validation[X, A]) = (f, a) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (Success(_), Failure(e)) => Failure(e)
      case (Failure(e), Success(_)) => Failure(e)
      case (Failure(e1), Failure(e2)) => Failure(e1 |+| e2)
    }
  }

  implicit def ValidationFailureApply[X] = new Apply[PartialApply1Of2[Validation.FailureProjection, X]#Flip] {
    def apply[A, B](f: Validation.FailureProjection[A => B, X], a: Validation.FailureProjection[A, X]) = ((f.validation, a.validation) match {
      case (Success(x1), Success(_)) => Success(x1)
      case (Success(x1), Failure(_)) => Success(x1)
      case (Failure(_), Success(x2)) => Success(x2)
      case (Failure(f), Failure(e)) => Failure(f(e))
    }).fail
  }

  implicit val ZipperApply: Apply[Zipper] = new Apply[Zipper] {
    def apply[A, B](f: Zipper[A => B], a: Zipper[A]): Zipper[B] =
      Zipper.zipper((a.lefts |!|) <*> (f.lefts |!|),
        (f.focus)(a.focus),
        (a.rights |!|) <*> (f.rights |!|))
  }

  implicit val ZipStreamApply: Apply[ZipStream] = new Apply[ZipStream] {
    def apply[A, B](f: ZipStream[A => B], a: ZipStream[A]): ZipStream[B] = {
      val ff = f.value
      val aa = a.value
      (if (ff.isEmpty || aa.isEmpty) Stream.empty
      else Stream.cons((ff.head)(aa.head), apply(ff.tail |!|, aa.tail |!|))) |!|
    }
  }

  val ZipTreeApply: Apply[Tree] = new Apply[Tree] {
    def apply[A, B](f: Tree[A => B], a: Tree[A]): Tree[B] = {
      Tree.node((f.rootLabel)(a.rootLabel), (a.subForest |!|) <*> (f.subForest.map((apply(_: Tree[A => B], _: Tree[A])).curry) |!|))
    }
  }

  implicit val TreeApply = FunctorBindApply[Tree]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListApply = FunctorBindApply[ArrayList]

  implicit val JavaLinkedListApply = FunctorBindApply[LinkedList]

  implicit val JavaPriorityQueueApply = FunctorBindApply[PriorityQueue]

  implicit val JavaStackApply = FunctorBindApply[Stack]

  implicit val JavaVectorApply = FunctorBindApply[Vector]

  implicit val JavaArrayBlockingQueueApply = FunctorBindApply[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueApply = FunctorBindApply[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListApply = FunctorBindApply[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueApply = FunctorBindApply[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueApply = FunctorBindApply[SynchronousQueue]
}
