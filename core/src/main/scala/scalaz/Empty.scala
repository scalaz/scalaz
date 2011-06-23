package scalaz

import scalaz.Scalaz._

trait Empty[F[_]] {
  def empty[A]: F[A]

  def deriving[G[_]](implicit n: ^**^[G, F]): Empty[G] =
    new Empty[G] {
      def empty[A] =
        n.pack(Empty.this.empty[A])
    }
}

object Empty extends Emptys

trait Emptys extends EmptysLow {
  implicit def OptionEmpty: Empty[Option] = new Empty[Option] {
    def empty[A] = None
  }

  implicit def ListEmpty: Empty[List] = new Empty[List] {
    def empty[A] = Nil
  }

  implicit def StreamEmpty: Empty[Stream] = new Empty[Stream] {
    def empty[A] = Stream.empty
  }

  implicit def MapEmpty[V: Semigroup]: Empty[({type λ[α] = Map[α, V]})#λ] = new Empty[({type λ[α] = Map[α, V]})#λ] {
    def empty[K] = Map.empty[K, V]
  }

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListEmpty: Empty[ArrayList] = new Empty[ArrayList] {
    def empty[A] = new ArrayList[A]
  }

  implicit def JavaHashSetEmpty: Empty[HashSet] = new Empty[HashSet] {
    def empty[A] = new HashSet[A]
  }

  implicit def JavaLinkedHashSetEmpty: Empty[LinkedHashSet] = new Empty[LinkedHashSet] {
    def empty[A] = new LinkedHashSet[A]
  }

  implicit def JavaLinkedListEmpty: Empty[LinkedList] = new Empty[LinkedList] {
    def empty[A] = new LinkedList[A]
  }

  implicit def JavaPriorityQueueEmpty: Empty[PriorityQueue] = new Empty[PriorityQueue] {
    def empty[A] = new PriorityQueue[A]
  }

  implicit def JavaStackEmpty: Empty[Stack] = new Empty[Stack] {
    def empty[A] = new Stack[A]
  }

  implicit def JavaTreeSetEmpty: Empty[TreeSet] = new Empty[TreeSet] {
    def empty[A] = new TreeSet[A]
  }

  implicit def JavaVectorEmpty: Empty[Vector] = new Empty[Vector] {
    def empty[A] = new Vector[A]
  }

  implicit def JavaArrayBlockingQueueEmpty: Empty[ArrayBlockingQueue] = new Empty[ArrayBlockingQueue] {
    def empty[A] = new ArrayBlockingQueue[A](0)
  }

  implicit def JavaConcurrentLinkedQueueEmpty: Empty[ConcurrentLinkedQueue] = new Empty[ConcurrentLinkedQueue] {
    def empty[A] = new ConcurrentLinkedQueue[A]
  }

  implicit def JavaCopyOnWriteArrayListEmpty: Empty[CopyOnWriteArrayList] = new Empty[CopyOnWriteArrayList] {
    def empty[A] = new CopyOnWriteArrayList[A]
  }

  implicit def JavaCopyOnWriteArraySetEmpty: Empty[CopyOnWriteArraySet] = new Empty[CopyOnWriteArraySet] {
    def empty[A] = new CopyOnWriteArraySet[A]
  }

  implicit def JavaLinkedBlockingQueueEmpty: Empty[LinkedBlockingQueue] = new Empty[LinkedBlockingQueue] {
    def empty[A] = new LinkedBlockingQueue[A]
  }

  implicit def JavaSynchronousQueueEmpty: Empty[SynchronousQueue] = new Empty[SynchronousQueue] {
    def empty[A] = new SynchronousQueue[A]
  }
}

trait EmptysLow {

  import collection.TraversableLike

  implicit def TraversableEmpty[CC[X] <: TraversableLike[X, CC[X]] : CanBuildAnySelf]: Empty[CC] = new Empty[CC] {
    def empty[A] = {
      val builder = implicitly[CanBuildAnySelf[CC]].apply[⊥, A]
      builder.result
    }
  }
}
