package scalaz

trait Monad[M[_]] extends Applicative[M] with Bind[M]

object Monad {
  def monad[M[_]](implicit b: Bind[M], p: Pure[M]) = new Monad[M] {
    def fmap[A, B](fa: M[A], f: A => B) = b.bind(fa, (a: A) => p.pure(f(a)))
    def apply[A, B](f: M[A => B], a: M[A]): M[B] = b.bind(f, (k: A => B) => fmap(a, k(_: A)))
    def pure[A](a: A) = p.pure(a)
    def bind[A, B](a: M[A], f: A => M[B]) = b.bind(a, f)
  }

  implicit val IdentityMonad: Monad[Identity] = monad[Identity]

  implicit def ContinuationMonad[R] = monad[PartialApply1Of2[Continuation, R]#Apply]

  implicit val NonEmptyListMonad = monad[NonEmptyList]

  implicit def StateMonad[S] = monad[PartialApply1Of2[State, S]#Apply]

  implicit val Tuple1Monad = monad[Tuple1]

  implicit val Function0Monad = monad[Function0]

  implicit def Function1Monad[R] = monad[PartialApply1Of2[Function1, R]#Apply]

  implicit def Function2Monad[R, S] = monad[PartialApply2Of3[Function2, R, S]#Apply]

  implicit def Function3Monad[R, S, T] = monad[PartialApply3Of4[Function3, R, S, T]#Apply]

  implicit def Function4Monad[R, S, T, U] = monad[PartialApply4Of5[Function4, R, S, T, U]#Apply]

  implicit def Function5Monad[R, S, T, U, V] = monad[PartialApply5Of6[Function5, R, S, T, U, V]#Apply]

  implicit def Function6Monad[R, S, T, U, V, W] = monad[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply]

  implicit val ListMonad = monad[List]

  implicit val StreamMonad = monad[Stream]

  implicit val OptionMonad = monad[Option]

  implicit val ArrayMonad = monad[Array]

  implicit def EitherLeftMonad[X] = monad[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightMonad[X] = monad[PartialApply1Of2[Either.RightProjection, X]#Apply]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListMonad = monad[ArrayList]

  implicit val JavaLinkedListMonad = monad[LinkedList]

  implicit val JavaPriorityQueueMonad = monad[PriorityQueue]

  implicit val JavaStackMonad = monad[Stack]

  implicit val JavaVectorMonad = monad[Vector]

  implicit val JavaArrayBlockingQueueMonad = monad[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueMonad = monad[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListMonad = monad[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueMonad = monad[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueMonad = monad[SynchronousQueue]
}
