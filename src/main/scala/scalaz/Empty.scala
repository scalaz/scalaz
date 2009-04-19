package scalaz

trait Empty[+E[_]] {
  def empty[A]: E[A]
}

object Empty {
  implicit val ListEmpty = new Empty[List] {
    def empty[A] = Nil
  }

  implicit val StreamEmpty = new Empty[Stream] {
    def empty[A] = Stream.empty
  }

  implicit val OptionEmpty = new Empty[Option] {
    def empty[A] = None
  }
  
  implicit val ArrayEmpty = new Empty[Array] {
    def empty[A] = new Array(0)
  }

  implicit def EitherLeftEmpty[X](implicit z: Zero[X]) = new Empty[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def empty[A] = Right(z.zero).left
  }

  implicit def EitherRightEmpty[X](implicit z: Zero[X]) = new Empty[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def empty[A] = Left(z.zero).right
  }

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListEmpty: Empty[ArrayList] = new Empty[ArrayList] {
    def empty[A] = new ArrayList[A]
  }

  implicit val JavaHashSetEmpty: Empty[HashSet] = new Empty[HashSet] {
    def empty[A] = new HashSet[A]
  }

  implicit val JavaLinkedHashSetEmpty: Empty[LinkedHashSet] = new Empty[LinkedHashSet] {
    def empty[A] = new LinkedHashSet[A]
  }

  implicit val JavaLinkedListEmpty: Empty[LinkedList] = new Empty[LinkedList] {
    def empty[A] = new LinkedList[A]
  }

  implicit val JavaPriorityQueueEmpty: Empty[PriorityQueue] = new Empty[PriorityQueue] {
    def empty[A] = new PriorityQueue[A]
  }

  implicit val JavaStackEmpty: Empty[Stack] = new Empty[Stack] {
    def empty[A] = new Stack[A]
  }

  implicit val JavaTreeSetEmpty: Empty[TreeSet] = new Empty[TreeSet] {
    def empty[A] = new TreeSet[A]
  }

  implicit val JavaVectorEmpty: Empty[Vector] = new Empty[Vector] {
    def empty[A] = new Vector[A]
  }

  implicit val JavaArrayBlockingQueueEmpty: Empty[ArrayBlockingQueue] = new Empty[ArrayBlockingQueue] {
    def empty[A] = new ArrayBlockingQueue[A](0)
  }

  implicit val JavaConcurrentLinkedQueueEmpty: Empty[ConcurrentLinkedQueue] = new Empty[ConcurrentLinkedQueue] {
    def empty[A] = new ConcurrentLinkedQueue[A]
  }

  implicit val JavaCopyOnWriteArrayListEmpty: Empty[CopyOnWriteArrayList] = new Empty[CopyOnWriteArrayList] {
    def empty[A] = new CopyOnWriteArrayList[A]
  }

  implicit val JavaCopyOnWriteArraySetEmpty: Empty[CopyOnWriteArraySet] = new Empty[CopyOnWriteArraySet] {
    def empty[A] = new CopyOnWriteArraySet[A]
  }

  implicit val JavaLinkedBlockingQueueEmpty: Empty[LinkedBlockingQueue] = new Empty[LinkedBlockingQueue] {
    def empty[A] = new LinkedBlockingQueue[A]
  }

  implicit val JavaSynchronousQueueEmpty: Empty[SynchronousQueue] = new Empty[SynchronousQueue] {
    def empty[A] = new SynchronousQueue[A]
  }
}
