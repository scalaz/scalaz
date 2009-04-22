package scalaz

trait Applicative[Z[_]] extends Pointed[Z] with Apply[Z] {
  override def fmap[A, B](fa: Z[A], f: A => B) = this(pure(f), fa)  
}

object Applicative {
  def applicative[Z[_]](implicit p: Pure[Z], a: Apply[Z]) = new Applicative[Z] {
    def pure[A](a: => A) = p.pure(a)
    def apply[A, B](f: Z[A => B], x: Z[A]) = a(f, x)
  }

  implicit val IdentityApplicative = applicative[Identity]

  implicit def ContinuationApplicative[R] = applicative[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListApplicative = applicative[NonEmptyList]

  implicit def StateApplicative[S] = applicative[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Applicative = applicative[Tuple1]

  implicit def Tuple2Applicative[R](implicit sr: Monoid[R]) = applicative[PartialApply1Of2[Tuple2, R]#Apply]

  implicit def Tuple3Applicative[R, S](implicit sr: Monoid[R], ss: Monoid[S]) = applicative[PartialApply2Of3[Tuple3, R, S]#Apply]

  implicit def Tuple4Applicative[R, S, T](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T]) = applicative[PartialApply3Of4[Tuple4, R, S, T]#Apply]

  implicit def Tuple5Applicative[R, S, T, U](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U]) = applicative[PartialApply4Of5[Tuple5, R, S, T, U]#Apply]

  implicit def Tuple6Applicative[R, S, T, U, V](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U], sv: Monoid[V]) = applicative[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply]

  implicit def Tuple7Applicative[R, S, T, U, V, W](implicit sr: Monoid[R], ss: Monoid[S], st: Monoid[T], su: Monoid[U], sv: Monoid[V], sw: Monoid[W]) = applicative[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply]

  implicit val Function0Applicative = applicative[Function0]

  implicit def Function1Applicative[R] = applicative[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Applicative[R, S] = applicative[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Applicative[R, S, T] = applicative[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Applicative[R, S, T, U] = applicative[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Applicative[R, S, T, U, V] = applicative[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Applicative[R, S, T, U, V, W] = applicative[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListApplicative = applicative[List]

  implicit val StreamApplicative = applicative[Stream]

  implicit val OptionApplicative = applicative[Option]

  implicit val ArrayApplicative = applicative[Array]

  implicit def EitherLeftApplicative[X] = applicative[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightApplicative[X] = applicative[PartialApply1Of2[Either.RightProjection, X]#Apply]

  implicit val ZipperApplicative = applicative[Zipper]

  implicit val ZipStreamApplicative = applicative[ZipStream]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListApplicative = applicative[ArrayList]

  implicit val JavaLinkedListApplicative = applicative[LinkedList]

  implicit val JavaPriorityQueueApplicative = applicative[PriorityQueue]

  implicit val JavaStackApplicative = applicative[Stack]

  implicit val JavaVectorApplicative = applicative[Vector]

  implicit val JavaArrayBlockingQueueApplicative = applicative[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueApplicative = applicative[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListApplicative = applicative[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueApplicative = applicative[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueApplicative = applicative[SynchronousQueue]
}
