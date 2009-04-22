package scalaz

trait MonadEmpty[M[_]] extends Monad[M] with Empty[M]

object MonadEmpty {
  def monadEmpty[M[_]](implicit m: Monad[M], e: Empty[M]) = new MonadEmpty[M] {
    def empty[A] = e.empty
    def pure[A](a: A) = m.pure(a)
    def bind[A, B](a: M[A], f: A => M[B]) = m.bind(a, f)    
  }

  implicit val ListMonadEmpty = monadEmpty[List]

  implicit val StreamMonadEmpty = monadEmpty[Stream]

  implicit val OptionMonadEmpty = monadEmpty[Option]

  implicit val ArrayMonadEmpty = monadEmpty[Array]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListMonadEmpty = monadEmpty[ArrayList]

  implicit val JavaLinkedListMonadEmpty = monadEmpty[LinkedList]

  implicit val JavaPriorityQueueMonadEmpty = monadEmpty[PriorityQueue]

  implicit val JavaStackMonadEmpty = monadEmpty[Stack]

  implicit val JavaVectorMonadEmpty = monadEmpty[Vector]

  implicit val JavaArrayBlockingQueueMonadEmpty = monadEmpty[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueMonadEmpty = monadEmpty[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListMonadEmpty = monadEmpty[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueMonadEmpty = monadEmpty[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueMonadEmpty = monadEmpty[SynchronousQueue]
}
