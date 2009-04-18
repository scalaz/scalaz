package scalaz

sealed trait MMA[M[_], A] {
  val v: M[M[A]]

  def join(implicit b: Bind[M]) = b.bind(v, (x: M[A]) => x)
}

object MMA {
  def mma[M[_]] = new PartialWrapMMA[M, MMA] {
    def apply[A](m: M[M[A]]) = new MMA[M, A] {
      val v = m
    }
  }

  implicit def IdentityMMA[A](a: Identity[Identity[A]]) = mma[Identity](a)

  implicit def ContinuationMMA[R, A](a: Continuation[R, Continuation[R, A]]) = mma[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def NonEmptyListMMA[A](a: NonEmptyList[NonEmptyList[A]]) = mma[NonEmptyList](a)

  implicit def StateMMA[S, A](a: State[S, State[S, A]]) = mma[PartialApply1Of2[State, S]#Apply](a)

  implicit def EqualMMA[A](a: Equal[Equal[A]]) = mma[Equal](a)

  implicit def OrderMMA[A](a: Order[Order[A]]) = mma[Order](a)

  implicit def ShowMMA[A](a: Show[Show[A]]) = mma[Show](a)

  implicit def Tuple1MMA[A](a: Tuple1[Tuple1[A]]) = mma[Tuple1](a)

  implicit def Tuple2MMA[R, A](a: (R, (R, A))) = mma[PartialApply1Of2[Tuple2, R]#Apply](a)

  implicit def Tuple3MMA[R, S, A](a: (R, S, (R, S, A))) = mma[PartialApply2Of3[Tuple3, R, S]#Apply](a)

  implicit def Tuple4MMA[R, S, T, A](a: (R, S, T, (R, S, T, A))) = mma[PartialApply3Of4[Tuple4, R, S, T]#Apply](a)

  implicit def Tuple5MMA[R, S, T, U, A](a: (R, S, T, U, (R, S, T, U, A))) = mma[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](a)

  implicit def Tuple6MMA[R, S, T, U, V, A](a: (R, S, T, U, V, (R, S, T, U, V, A))) = mma[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](a)

  implicit def Tuple7MMA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, (R, S, T, U, V, W, A))) = mma[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](a)

  implicit def Function0MMA[A](a: Function0[Function0[A]]) = mma[Function0](a)

  implicit def Function1MMA[R, A](a: R => R => A) = mma[PartialApply1Of2[Function1, R]#Apply](a)

  implicit def Function2MMA[R, S, A](a: (R, S) => (R, S) => A) = mma[PartialApply2Of3[Function2, R, S]#Apply](a)

  implicit def Function3MMA[R, S, T, A](a: (R, S, T) => (R, S, T) => A) = mma[PartialApply3Of4[Function3, R, S, T]#Apply](a)

  implicit def Function4MMA[R, S, T, U, A](a: (R, S, T, U) => (R, S, T, U) => A) = mma[PartialApply4Of5[Function4, R, S, T, U]#Apply](a)

  implicit def Function5MMA[R, S, T, U, V, A](a: (R, S, T, U, V) => (R, S, T, U, V) => A) = mma[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](a)

  implicit def Function6MMA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => (R, S, T, U, V, W) => A) = mma[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](a)

  implicit def ListMMA[A](a: List[List[A]]) = mma[List](a)

  implicit def StreamMMA[A](a: Stream[Stream[A]]) = mma[Stream](a)

  implicit def OptionMMA[A](a: Option[Option[A]]) = mma[Option](a)

  implicit def ArrayMMA[A](a: Array[Array[A]]) = mma[Array](a)

  implicit def EitherLeftMMA[X, A](a: Either.LeftProjection[Either.LeftProjection[A, X], X]) = mma[PartialApply1Of2[Either.LeftProjection, X]#Flip](a)

  implicit def EitherRightMMA[X, A](a: Either.RightProjection[X, Either.RightProjection[X, A]]) = mma[PartialApply1Of2[Either.RightProjection, X]#Apply](a)

  import java.util._
  import java.util.concurrent._

  implicit def ArrayListMMMA[A](a: ArrayList[ArrayList[A]]) = mma[ArrayList](a)

  implicit def HashSetMMMA[A](a: HashSet[HashSet[A]]) = mma[HashSet](a)

  implicit def LinkedHashSetMMMA[A](a: LinkedHashSet[LinkedHashSet[A]]) = mma[LinkedHashSet](a)

  implicit def LinkedListMMMA[A](a: LinkedList[LinkedList[A]]) = mma[LinkedList](a)

  implicit def PriorityQueueMMMA[A](a: PriorityQueue[PriorityQueue[A]]) = mma[PriorityQueue](a)

  implicit def StackMMMA[A](a: Stack[Stack[A]]) = mma[Stack](a)

  implicit def TreeSetMMMA[A](a: TreeSet[TreeSet[A]]) = mma[TreeSet](a)

  implicit def VectorMMMA[A](a: Vector[Vector[A]]) = mma[Vector](a)

  implicit def ArrayBlockingQueueMMMA[A](a: ArrayBlockingQueue[ArrayBlockingQueue[A]]) = mma[ArrayBlockingQueue](a)

  implicit def ConcurrentLinkedQueueMMMA[A](a: ConcurrentLinkedQueue[ConcurrentLinkedQueue[A]]) = mma[ConcurrentLinkedQueue](a)

  implicit def CopyOnWriteArrayListMMMA[A](a: CopyOnWriteArrayList[CopyOnWriteArrayList[A]]) = mma[CopyOnWriteArrayList](a)

  implicit def CopyOnWriteArraySetMMMA[A](a: CopyOnWriteArraySet[CopyOnWriteArraySet[A]]) = mma[CopyOnWriteArraySet](a)

  implicit def LinkedBlockingQueueMMMA[A](a: LinkedBlockingQueue[LinkedBlockingQueue[A]]) = mma[LinkedBlockingQueue](a)

  implicit def PriorityBlockingQueueMMMA[A](a: PriorityBlockingQueue[PriorityBlockingQueue[A]]) = mma[PriorityBlockingQueue](a)

  implicit def SynchronousQueueMMMA[A](a: SynchronousQueue[SynchronousQueue[A]]) = mma[SynchronousQueue](a)
}
