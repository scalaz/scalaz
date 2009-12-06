package scalaz

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

trait Applys {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      b.bind(f, (g: A => B) => t.fmap(a, g(_: A)))
    }
  }  
}

object Apply {
  import Scalaz._

  implicit def IdentityApply: Apply[Identity] = FunctorBindApply[Identity]

  implicit def NonEmptyListApply: Apply[NonEmptyList] = FunctorBindApply[NonEmptyList]

  implicit def StateApply[S]: Apply[PartialApply1Of2[State, S]#Apply] = FunctorBindApply[PartialApply1Of2[State, S]#Apply]

  implicit def Tuple1Apply: Apply[Tuple1] = FunctorBindApply[Tuple1]

  implicit def Tuple2Apply[R](implicit sr: Semigroup[R]): Apply[PartialApply1Of2[Tuple2, R]#Apply] = FunctorBindApply[PartialApply1Of2[Tuple2, R]#Apply]

  implicit def Tuple3Apply[R, S](implicit sr: Semigroup[R], ss: Semigroup[S]): Apply[PartialApply2Of3[Tuple3, R, S]#Apply] = FunctorBindApply[PartialApply2Of3[Tuple3, R, S]#Apply]

  implicit def Tuple4Apply[R, S, T](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T]): Apply[PartialApply3Of4[Tuple4, R, S, T]#Apply] = FunctorBindApply[PartialApply3Of4[Tuple4, R, S, T]#Apply]

  implicit def Tuple5Apply[R, S, T, U](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U]): Apply[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] = FunctorBindApply[PartialApply4Of5[Tuple5, R, S, T, U]#Apply]

  implicit def Tuple6Apply[R, S, T, U, V](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U], sv: Semigroup[V]): Apply[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] = FunctorBindApply[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply]

  implicit def Tuple7Apply[R, S, T, U, V, W](implicit sr: Semigroup[R], ss: Semigroup[S], st: Semigroup[T], su: Semigroup[U], sv: Semigroup[V], sw: Semigroup[W]): Apply[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] = FunctorBindApply[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply]

  implicit def Function0Apply: Apply[Function0] = FunctorBindApply[Function0]

  implicit def Function1Apply[R]: Apply[PartialApply1Of2[Function1, R]#Apply] = FunctorBindApply[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Apply[R, S]: Apply[PartialApply2Of3[Function2, R, S]#Apply] = FunctorBindApply[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Apply[R, S, T]: Apply[PartialApply3Of4[Function3, R, S, T]#Apply] = FunctorBindApply[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Apply[R, S, T, U]: Apply[PartialApply4Of5[Function4, R, S, T, U]#Apply] = FunctorBindApply[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Apply[R, S, T, U, V]: Apply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] = FunctorBindApply[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Apply[R, S, T, U, V, W]: Apply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] = FunctorBindApply[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit def ListApply: Apply[List] = FunctorBindApply[List]

  implicit def StreamApply: Apply[Stream] = FunctorBindApply[Stream]

  implicit def OptionApply: Apply[Option] = FunctorBindApply[Option]

  implicit def GenericArrayApply: Apply[GArray] = FunctorBindApply[GArray]

  implicit def EitherLeftApply[X]: Apply[PartialApply1Of2[Either.LeftProjection, X]#Flip] = FunctorBindApply[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightApply[X]: Apply[PartialApply1Of2[Either.RightProjection, X]#Apply] = FunctorBindApply[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit def ResponderApply: Apply[Responder] = FunctorBindApply[Responder]

  import java.util.concurrent.Callable

  implicit def CallableApply: Apply[Callable] = FunctorBindApply[Callable]

  import java.util.Map.Entry

  implicit def MapEntryApply[X](implicit sr: Semigroup[X]): Apply[PartialApply1Of2[Entry, X]#Apply] = FunctorBindApply[PartialApply1Of2[Entry, X]#Apply]

  implicit def ValidationApply[X](implicit s: Semigroup[X]): Apply[PartialApply1Of2[Validation, X]#Apply] = new Apply[PartialApply1Of2[Validation, X]#Apply] {
    def apply[A, B](f: Validation[X, A => B], a: Validation[X, A]) = (f, a) match {
      case (Success(f), Success(a)) => success(f(a))
      case (Success(_), Failure(e)) => failure(e)
      case (Failure(e), Success(_)) => failure(e)
      case (Failure(e1), Failure(e2)) => failure(e1 ⊹ e2)
    }
  }

  implicit def ValidationFailureApply[X]: Apply[PartialApply1Of2[FailProjection, X]#Flip] = new Apply[PartialApply1Of2[FailProjection, X]#Flip] {
    def apply[A, B](f: FailProjection[A => B, X], a: FailProjection[A, X]) = ((f.validation, a.validation) match {
      case (Success(x1), Success(_)) => success(x1)
      case (Success(x1), Failure(_)) => success(x1)
      case (Failure(_), Success(x2)) => success(x2)
      case (Failure(f), Failure(e)) => failure(f(e))
    }).fail
  }

  implicit def ZipperApply: Apply[Zipper] = new Apply[Zipper] {
    def apply[A, B](f: Zipper[A => B], a: Zipper[A]): Zipper[B] =
      zipper((a.lefts ʐ) ⊛ (f.lefts ʐ),
        (f.focus)(a.focus),
        (a.rights ʐ) ⊛ (f.rights ʐ))
  }

  implicit def ZipStreamApply: Apply[ZipStream] = new Apply[ZipStream] {
    def apply[A, B](f: ZipStream[A => B], a: ZipStream[A]): ZipStream[B] = {
      val ff = f.value
      val aa = a.value
      (if (ff.isEmpty || aa.isEmpty) Stream.empty
      else Stream.cons((ff.head)(aa.head), apply(ff.tail ʐ, aa.tail ʐ))) ʐ
    }
  }

  val ZipTreeApply: Apply[Tree] = new Apply[Tree] {
    def apply[A, B](f: Tree[A => B], a: Tree[A]): Tree[B] =
      node((f.rootLabel)(a.rootLabel), (a.subForest ʐ) ⊛ (f.subForest.map((apply(_: Tree[A => B], _: Tree[A])).curry) ʐ))
  }

  implicit def TreeApply = FunctorBindApply[Tree]

  import concurrent.Promise
  implicit def PromiseApply = FunctorBindApply[Promise]

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListApply: Apply[ArrayList] = FunctorBindApply[ArrayList]

  implicit def JavaLinkedListApply: Apply[LinkedList] = FunctorBindApply[LinkedList]

  implicit def JavaPriorityQueueApply: Apply[PriorityQueue] = FunctorBindApply[PriorityQueue]

  implicit def JavaStackApply: Apply[Stack] = FunctorBindApply[Stack]

  implicit def JavaVectorApply: Apply[Vector] = FunctorBindApply[Vector]

  implicit def JavaArrayBlockingQueueApply: Apply[ArrayBlockingQueue] = FunctorBindApply[ArrayBlockingQueue]

  implicit def JavaConcurrentLinkedQueueApply: Apply[ConcurrentLinkedQueue] = FunctorBindApply[ConcurrentLinkedQueue]

  implicit def JavaCopyOnWriteArrayListApply: Apply[CopyOnWriteArrayList] = FunctorBindApply[CopyOnWriteArrayList]

  implicit def JavaLinkedBlockingQueueApply: Apply[LinkedBlockingQueue] = FunctorBindApply[LinkedBlockingQueue]

  implicit def JavaSynchronousQueueApply: Apply[SynchronousQueue] = FunctorBindApply[SynchronousQueue]
}
