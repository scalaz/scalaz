package scalaz

trait MonadPlus[M[_]] extends Monad[M] with Plus[M]

object MonadPlus {
  def monadPlus[M[_]](implicit m: Monad[M], p: Plus[M]) = new MonadPlus[M] {
    def plus[A](a1: M[A], a2: => M[A]) = p.plus(a1, a2)
    def pure[A](a: A) = m.pure(a)
    def bind[A, B](a: M[A], f: A => M[B]) = m.bind(a, f)
  }

  implicit val NonEmptyListMonadPlus = monadPlus[NonEmptyList]

  implicit val ListMonadPlus = monadPlus[List]

  implicit val StreamMonadPlus = monadPlus[Stream]

  implicit val OptionMonadPlus = monadPlus[Option]

  implicit val ArrayMonadPlus = monadPlus[Array]

  implicit def EitherLeftMonadPlus[X] = monadPlus[PartialApply1Of2[Either.LeftProjection, X]#Flip]

  implicit def EitherRightMonadPlus[X] = monadPlus[PartialApply1Of2[Either.RightProjection, X]#Apply]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListMonadPlus = monadPlus[ArrayList]

  implicit val JavaLinkedListMonadPlus = monadPlus[LinkedList]

  implicit val JavaPriorityQueueMonadPlus = monadPlus[PriorityQueue]

  implicit val JavaStackMonadPlus = monadPlus[Stack]

  implicit val JavaVectorMonadPlus = monadPlus[Vector]

  implicit val JavaArrayBlockingQueueMonadPlus = monadPlus[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueMonadPlus = monadPlus[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListMonadPlus = monadPlus[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueMonadPlus = monadPlus[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueMonadPlus = monadPlus[SynchronousQueue]
}
