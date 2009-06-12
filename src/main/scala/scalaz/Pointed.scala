package scalaz

trait Pointed[P[_]] extends Functor[P] with Pure[P]

object Pointed {
  def pointed[P[_]](implicit t: Functor[P], p: Pure[P]) = new Pointed[P] {
    def fmap[A, B](a: P[A], f: A => B) = t.fmap(a, f)

    def pure[A](a: => A): P[A] = p.pure(a)
  }

  implicit val IdentityPointed: Pointed[Identity] = pointed[Identity]

  implicit def ContinuationPointed[R] = pointed[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListPointed = pointed[NonEmptyList]

  implicit def StatePointed[S] = pointed[PartialApply1Of2[State, S]#Apply]

  implicit val ZipStreamPointed = pointed[ZipStream]

  implicit val Tuple1Pointed = pointed[Tuple1]

  implicit def Tuple2Pointed[R](implicit sr: Monoid[R]) = pointed[PartialApply1Of2[Tuple2, R]#Apply]

  implicit def Tuple3Pointed[R, S](implicit sr: Monoid[R], ss: Monoid[S]) = pointed[PartialApply2Of3[Tuple3, R, S]#Apply]

  implicit def Tuple4Pointed[R, S, T](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T]) = pointed[PartialApply3Of4[Tuple4, R, S, T]#Apply]

  implicit def Tuple5Pointed[R, S, T, U](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U]) = pointed[PartialApply4Of5[Tuple5, R, S, T, U]#Apply]

  implicit def Tuple6Pointed[R, S, T, U, V](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U], sv: Monoid[V]) = pointed[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply]

  implicit def Tuple7Pointed[R, S, T, U, V, W](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U], sv: Monoid[V], sw: Monoid[W]) = pointed[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply]

  implicit val Function0Pointed = pointed[Function0]

  implicit def Function1Pointed[R] = pointed[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Pointed[R, S] = pointed[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Pointed[R, S, T] = pointed[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Pointed[R, S, T, U] = pointed[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Pointed[R, S, T, U, V] = pointed[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Pointed[R, S, T, U, V, W] = pointed[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListPointed = pointed[List]

  implicit val StreamPointed = pointed[Stream]

  implicit val OptionPointed = pointed[Option]

  implicit val ArrayPointed = pointed[Array]

  implicit def EitherLeftPointed[X] = pointed[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightPointed[X] = pointed[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit def ValidationPointed[X] = pointed[PartialApply1Of2[Validation, X]#Apply]

  implicit def ValidationFailurePointed[X] = pointed[PartialApply1Of2[Validation.FailureProjection, X]#Flip]

  implicit val ZipperPointed = pointed[Zipper]

  implicit val TreePointed = pointed[Tree]

  implicit val TreeLocPointed = pointed[TreeLoc]

  import concurrent._
  implicit def PromisePointed(implicit s: Strategy[Unit]) = pointed[Promise]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListPointed = pointed[ArrayList]

  implicit val JavaLinkedListPointed = pointed[LinkedList]

  implicit val JavaPriorityQueuePointed = pointed[PriorityQueue]

  implicit val JavaStackPointed = pointed[Stack]

  implicit val JavaVectorPointed = pointed[Vector]

  implicit val JavaArrayBlockingQueuePointed = pointed[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueuePointed = pointed[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListPointed = pointed[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueuePointed = pointed[LinkedBlockingQueue]

  implicit val JavaSynchronousQueuePointed = pointed[SynchronousQueue]

  import org.scalacheck.{Gen, Arbitrary}

  implicit val GenPointed = pointed[Gen]

  implicit val ArbitraryPointed = pointed[Arbitrary]
}
