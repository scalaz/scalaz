package scalaz

trait Pure[+P[_]] {
  def pure[A](a: A): P[A]
}

object Pure {
  import S._

  implicit val IdentityPure: Pure[Identity] = new Pure[Identity] {
    def pure[A](a: A) = Identity.id(a)
  }

  implicit def ContinuationPure[R] = new Pure[PartialApply1Of2[Continuation, R]#Apply] {
    def pure[A](a: A) = Continuation.continuation[R, A](_(a))
  }

  implicit val NonEmptyListPure = new Pure[NonEmptyList] {
    def pure[A](a: A) = NonEmptyList.nel(a)
  }

  implicit def StatePure[S] = new Pure[PartialApply1Of2[State, S]#Apply] {
    def pure[A](a: A) = a.state[S]
  }

  implicit val Tuple1Pure = new Pure[Tuple1] {
    def pure[A](a: A) = Tuple1(a)
  }

  implicit def Tuple2Pure[R](implicit zr: Zero[R]) = new Pure[PartialApply1Of2[Tuple2, R]#Apply] {
    def pure[A](a: A) = (zr.zero, a)
  }

  implicit def Tuple3Pure[R, S](implicit zr: Zero[R], zs: Zero[S]) = new Pure[PartialApply2Of3[Tuple3, R, S]#Apply] {
    def pure[A](a: A) = (zr.zero, zs.zero, a)
  }

  implicit def Tuple4Pure[R, S, T](implicit zr: Zero[R], zs: Zero[S], zt: Zero[T]) = new Pure[PartialApply3Of4[Tuple4, R, S, T]#Apply] {
    def pure[A](a: A) = (zr.zero, zs.zero, zt.zero, a)
  }

  implicit def Tuple5Pure[R, S, T, U](implicit zr: Zero[R], zs: Zero[S], zt: Zero[T], zu: Zero[U]) = new Pure[PartialApply4Of5[Tuple5, R, S, T, U]#Apply] {
    def pure[A](a: A) = (zr.zero, zs.zero, zt.zero, zu.zero, a)
  }

  implicit def Tuple6Pure[R, S, T, U, V](implicit zr: Zero[R], zs: Zero[S], zt: Zero[T], zu: Zero[U], zv: Zero[V]) = new Pure[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply] {
    def pure[A](a: A) = (zr.zero, zs.zero, zt.zero, zu.zero, zv.zero, a)
  }

  implicit def Tuple7Pure[R, S, T, U, V, W](implicit zr: Zero[R], zs: Zero[S], zt: Zero[T], zu: Zero[U], zv: Zero[V], zw: Zero[W]) = new Pure[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply] {
    def pure[A](a: A) = (zr.zero, zs.zero, zt.zero, zu.zero, zv.zero, zw.zero, a)
  }

  implicit val Function0Pure = new Pure[Function0] {
    def pure[A](a: A) = new Function0[A] {
      def apply = a
    }
  }

  implicit def Function1Pure[R] = new Pure[PartialApply1Of2[Function1, R]#Apply] {
    def pure[A](a: A) = (_: R) => a
  }

  implicit def Function2Pure[R, S] = new Pure[PartialApply2Of3[Function2, R, S]#Apply] {
    def pure[A](a: A) = (_: R, _: S) => a
  }

  implicit def Function3Pure[R, S, T] = new Pure[PartialApply3Of4[Function3, R, S, T]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T ) => a
  }

  implicit def Function4Pure[R, S, T, U] = new Pure[PartialApply4Of5[Function4, R, S, T, U]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T , _: U) => a
  }

  implicit def Function5Pure[R, S, T, U, V] = new Pure[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T , _: U, _: V) => a
  }

  implicit def Function6Pure[R, S, T, U, V, W] = new Pure[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] {
    def pure[A](a: A) = (_: R, _: S , _: T , _: U, _: V, _: W) => a
  }

  implicit val ListPure = new Pure[List] {
    def pure[A](a: A) = List(a)
  }

  implicit val StreamPure = new Pure[Stream] {
    def pure[A](a: A) = Stream(a)
  }

  implicit val OptionPure = new Pure[Option] {
    def pure[A](a: A) = Some(a)
  }

  implicit val ArrayPure = new Pure[Array] {
    def pure[A](a: A) = Array.make(1, a)
  }

  implicit def EitherLeftPure[X] = new Pure[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def pure[A](a: A) = Left(a).left
  }

  implicit def EitherRightPure[X] = new Pure[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def pure[A](a: A) = Right(a).right
  }

  implicit def ZipperPure = new Pure[Zipper] {
    def pure[A](a: A) = Zipper.zipper(a)
  }

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListPure: Pure[ArrayList] = new Pure[ArrayList] {
    def pure[A](a: A) = {
      val k = new ArrayList[A]
      k add a
      k
    }
  }

  implicit val JavaHashSetPure: Pure[HashSet] = new Pure[HashSet] {
    def pure[A](a: A) = {
      val k = new HashSet[A]
      k add a
      k
    }
  }

  implicit val JavaLinkedHashSetPure: Pure[LinkedHashSet] = new Pure[LinkedHashSet] {
    def pure[A](a: A) = {
      val k = new LinkedHashSet[A]
      k add a
      k
    }
  }

  implicit val JavaLinkedListPure: Pure[LinkedList] = new Pure[LinkedList] {
    def pure[A](a: A) = {
      val k = new LinkedList[A]
      k add a
      k
    }
  }

  implicit val JavaPriorityQueuePure: Pure[PriorityQueue] = new Pure[PriorityQueue] {
    def pure[A](a: A) = {
      val k = new PriorityQueue[A]
      k add a
      k
    }
  }

  implicit val JavaStackPure: Pure[Stack] = new Pure[Stack] {
    def pure[A](a: A) = {
      val k = new Stack[A]
      k add a
      k
    }
  }

  implicit val JavaTreeSetPure: Pure[TreeSet] = new Pure[TreeSet] {
    def pure[A](a: A) = {
      val k = new TreeSet[A]
      k add a
      k
    }
  }

  implicit val JavaVectorPure: Pure[Vector] = new Pure[Vector] {
    def pure[A](a: A) = {
      val k = new Vector[A]
      k add a
      k
    }
  }

  implicit val JavaArrayBlockingQueuePure: Pure[ArrayBlockingQueue] = new Pure[ArrayBlockingQueue] {
    def pure[A](a: A) = {
      val k = new ArrayBlockingQueue[A](0)
      k add a
      k
    }
  }

  implicit val JavaConcurrentLinkedQueuePure: Pure[ConcurrentLinkedQueue] = new Pure[ConcurrentLinkedQueue] {
    def pure[A](a: A) = {
      val k = new ConcurrentLinkedQueue[A]
      k add a
      k
    }
  }

  implicit val JavaCopyOnWriteArrayListPure: Pure[CopyOnWriteArrayList] = new Pure[CopyOnWriteArrayList] {
    def pure[A](a: A) = {
      val k = new CopyOnWriteArrayList[A]
      k add a
      k
    }
  }

  implicit val JavaCopyOnWriteArraySetPure: Pure[CopyOnWriteArraySet] = new Pure[CopyOnWriteArraySet] {
    def pure[A](a: A) = {
      val k = new CopyOnWriteArraySet[A]
      k add a
      k
    }
  }

  implicit val JavaLinkedBlockingQueuePure: Pure[LinkedBlockingQueue] = new Pure[LinkedBlockingQueue] {
    def pure[A](a: A) = {
      val k = new LinkedBlockingQueue[A]
      k add a
      k
    }
  }

  implicit val JavaSynchronousQueuePure: Pure[SynchronousQueue] = new Pure[SynchronousQueue] {
    def pure[A](a: A) = {
      val k = new SynchronousQueue[A]
      k add a
      k
    }
  }
}
