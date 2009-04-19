package scalaz

trait Bind[Z[_]] {
  def bind[A, B](a: Z[A], f: A => Z[B]): Z[B]
}

object Bind {
  implicit val IdentityBind: Bind[Identity] = new Bind[Identity] {
    def bind[A, B](a: Identity[A], f: A => Identity[B]) = f(a.value)
  }

  implicit def ContinuationBind[R]: Bind[PartialApply1Of2[Continuation, R]#Apply] = new Bind[PartialApply1Of2[Continuation, R]#Apply] {
    def bind[A, B](a: Continuation[R, A], f: A => Continuation[R, B]) = Continuation.continuation[R, B](c => a(p => f(p)(c)))
  }

  implicit val NonEmptyListBind = new Bind[NonEmptyList] {
    def bind[A, B](r: NonEmptyList[A], f: A => NonEmptyList[B]) = r flatMap f
  }

  implicit def StateBind[S] = new Bind[PartialApply1Of2[State, S]#Apply] {
    def bind[A, B](r: State[S, A], f: A => State[S, B]) = r flatMap f
  }

  implicit val Tuple1Bind = new Bind[Tuple1] {
    def bind[A, B](r: Tuple1[A], f: A => Tuple1[B]) = f(r._1)
  }

  implicit val Function0Bind = new Bind[Function0] {
    def bind[A, B](r: Function0[A], f: A => Function0[B]) = f(r.apply)
  }

  implicit def Function1Bind[R] = new Bind[PartialApply1Of2[Function1, R]#Apply] {
    def bind[A, B](r: R => A, f: A => R => B) = (t: R) => f(r(t))(t)
  }

  implicit def Function2Bind[R, S] = new Bind[PartialApply2Of3[Function2, R, S]#Apply] {
    def bind[A, B](r: (R, S) => A, f: A => (R, S) => B) = (t1: R, t2: S) => f(r(t1, t2))(t1, t2)
  }

  implicit def Function3Bind[R, S, T] = new Bind[PartialApply3Of4[Function3, R, S, T]#Apply] {
    def bind[A, B](r: (R, S, T) => A, f: A => (R, S, T) => B) = (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))(t1, t2, t3)
  }

  implicit def Function4Bind[R, S, T, U] = new Bind[PartialApply4Of5[Function4, R, S, T, U]#Apply] {
    def bind[A, B](r: (R, S, T, U) => A, f: A => (R, S, T, U) => B) = (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))(t1, t2, t3, t4)
  }

  implicit def Function5Bind[R, S, T, U, V] = new Bind[PartialApply5Of6[Function5, R, S, T, U, V]#Apply] {
    def bind[A, B](r: (R, S, T, U, V) => A, f: A => (R, S, T, U, V) => B) = (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }

  implicit def Function6Bind[R, S, T, U, V, W] = new Bind[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply] {
    def bind[A, B](r: (R, S, T, U, V, W) => A, f: A => (R, S, T, U, V, W) => B) = (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }

  implicit val ListBind = new Bind[List] {
    def bind[A, B](r: List[A], f: A => List[B]) = r flatMap f
  }

  implicit val StreamBind = new Bind[Stream] {
    def bind[A, B](r: Stream[A], f: A => Stream[B]) = r flatMap f
  }

  implicit val OptionBind = new Bind[Option] {
    def bind[A, B](r: Option[A], f: A => Option[B]) = r flatMap f
  }

  implicit val ArrayBind = new Bind[Array] {
    def bind[A, B](r: Array[A], f: A => Array[B]) = r flatMap f
  }

  implicit def EitherLeftBind[X] = new Bind[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def bind[A, B](r: Either.LeftProjection[A, X], f: A => Either.LeftProjection[B, X]) = r.flatMap(f(_).e).left
  }

  implicit def EitherRightBind[X] = new Bind[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def bind[A, B](r: Either.RightProjection[X, A], f: A => Either.RightProjection[X, B]) = r.flatMap(f(_).e).right
  }

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListBind: Bind[ArrayList] = new Bind[ArrayList] {
    def bind[A, B](r: ArrayList[A], f: A => ArrayList[B]) = {
      val a = new ArrayList[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaLinkedListBind: Bind[LinkedList] = new Bind[LinkedList] {
    def bind[A, B](r: LinkedList[A], f: A => LinkedList[B]) = {
      val a = new LinkedList[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaPriorityQueueBind: Bind[PriorityQueue] = new Bind[PriorityQueue] {
    def bind[A, B](r: PriorityQueue[A], f: A => PriorityQueue[B]) = {
      val a = new PriorityQueue[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaStackBind: Bind[Stack] = new Bind[Stack] {
    def bind[A, B](r: Stack[A], f: A => Stack[B]) = {
      val a = new Stack[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaVectorBind: Bind[Vector] = new Bind[Vector] {
    def bind[A, B](r: Vector[A], f: A => Vector[B]) = {
      val a = new Vector[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaArrayBlockingQueueBind: Bind[ArrayBlockingQueue] = new Bind[ArrayBlockingQueue] {
    def bind[A, B](r: ArrayBlockingQueue[A], f: A => ArrayBlockingQueue[B]) = {
      val a = new ArrayBlockingQueue[B](r.remainingCapacity)
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaConcurrentLinkedQueueBind: Bind[ConcurrentLinkedQueue] = new Bind[ConcurrentLinkedQueue] {
    def bind[A, B](r: ConcurrentLinkedQueue[A], f: A => ConcurrentLinkedQueue[B]) = {
      val a = new ConcurrentLinkedQueue[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaCopyOnWriteArrayListBind: Bind[CopyOnWriteArrayList] = new Bind[CopyOnWriteArrayList] {
    def bind[A, B](r: CopyOnWriteArrayList[A], f: A => CopyOnWriteArrayList[B]) = {
      val a = new CopyOnWriteArrayList[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaLinkedBlockingQueueBind: Bind[LinkedBlockingQueue] = new Bind[LinkedBlockingQueue] {
    def bind[A, B](r: LinkedBlockingQueue[A], f: A => LinkedBlockingQueue[B]) = {
      val a = new LinkedBlockingQueue[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }

  implicit val JavaSynchronousQueueBind: Bind[SynchronousQueue] = new Bind[SynchronousQueue] {
    def bind[A, B](r: SynchronousQueue[A], f: A => SynchronousQueue[B]) = {
      val a = new SynchronousQueue[B]
      val i = r.iterator
      while(i.hasNext)
        a.addAll(f(i.next))
      a
    }
  }
}
