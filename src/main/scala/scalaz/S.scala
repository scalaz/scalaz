package scalaz


import concurrent.Strategy

object S {
  import java.math.BigInteger

  def id[A](f:() => A) = LazyIdentity.id(f)
  
  implicit def IdentityTo[A](x: A) = Identity.IdentityTo(x)
  
  implicit def ArrayByteTo(bs: Array[Byte]) = ArrayByte.ArrayByteTo(bs)

  implicit def ArrayByteFrom(bs: ArrayByte) = ArrayByte.ArrayByteFrom(bs)

  implicit def BigIntTo(n: BigInt) = BigIntW.BigIntTo(n)

  implicit def BigIntFrom(n: BigIntW) = BigIntW.BigIntFrom(n)

  implicit def BigIntegerTo(n: BigInteger) = BigIntegerW.BigIntegerTo(n)

  implicit def BigIntegerFrom(n: BigIntegerW) = BigIntegerW.BigIntegerFrom(n)

  implicit def BooleanTo(b: Boolean) = BooleanW.BooleanTo(b)

  implicit def BooleanFrom(b: BooleanW) = BooleanW.BooleanFrom(b)

  def conjunction(b: Boolean) = BooleanConjunction.conjunction(b)

  implicit def BooleanConjunctionFrom(b: BooleanConjunction) = BooleanConjunction.BooleanConjunctionFrom(b)

  def multiplication(n: Byte) = ByteMultiplication.multiplication(n)

  implicit def ByteMultiplicationFrom(n: ByteMultiplication) = ByteMultiplication.ByteMultiplicationFrom(n)

  implicit def ByteTo(n: Byte) = ByteW.ByteTo(n)

  implicit def ByteFrom(n: ByteW) = ByteW.ByteFrom(n)

  def multiplication(n: Char) = CharMultiplication.multiplication(n)

  implicit def CharMultiplicationFrom(n: CharMultiplication) = CharMultiplication.CharMultiplicationFrom(n)

  implicit def CharTo(c: Char) = CharW.CharTo(c)

  implicit def CharFrom(c: CharW) = CharW.CharFrom(c)

  def cokleisli[W[_]] = Cokleisli.cokleisli[W]

  def continuation[R, A](f: (A => R) => R) = Continuation.continuation(f)

  implicit def DigitLong(d: Digit) = Digit.DigitLong(d)

  implicit def LongDigit(n: Long) = Digit.LongDigit(n)

  implicit def DoubleTo(n: Double) = DoubleW.DoubleTo(n)

  implicit def DoubleFrom(n: DoubleW) = DoubleW.DoubleFrom(n)

  def multiplication(n: Long) = LongMultiplication.multiplication(n)

  implicit def EndoTo[A](f: A => A) = Endo.EndoTo(f)

  implicit def EndoFrom[A](e: Endo[A]) = Endo.EndoFrom(e)

  implicit def LongMultiplicationFrom(n: LongMultiplication) = LongMultiplication.LongMultiplicationFrom(n)

  implicit def Function1To[T, R](f: T => R) = Function1W.Function1To(f)

  implicit def Function1From[T, R](f: Function1W[T, R]) = Function1W.Function1From(f)

  implicit def Function2To[T1, T2, R](f: (T1, T2) => R) = Function2W.Function2To(f)

  implicit def Function2From[T1, T2, R](f: Function2W[T1, T2, R]) = Function2W.Function2From(f)

  def multiplication(n: Int) = IntMultiplication.multiplication(n)

  implicit def IntMultiplicationFrom(n: IntMultiplication) = IntMultiplication.IntMultiplicationFrom(n)

  implicit def IntTo(n: Int) = IntW.IntTo(n)

  implicit def IntFrom(n: IntW) = IntW.IntFrom(n)

  implicit def IterableTo[A](i: Iterable[A]) = IterableW.IterableTo(i)

  implicit def IterableFrom[A](i: IterableW[A]) = IterableW.IterableFrom(i)

  implicit def JavaIterableTo[A](i: java.lang.Iterable[A]): IterableW[A] = IterableW.JavaIterableTo(i)

  def kleisli[M[_]] = Kleisli.kleisli[M]

  implicit def LazyIdentityTo[A](a: A) = LazyIdentity.LazyIdentityTo(a)

  implicit def LazyIdentityFrom[A](i: LazyIdentity[A]) = LazyIdentity.LazyIdentityFrom(i)

  implicit def ListTo[A](as: List[A]): ListW[A] = ListW.ListTo(as)

  implicit def ListFrom[A](as: ListW[A]) = ListW.ListFrom(as)

  implicit def LongTo(n: Long) = LongW.LongTo(n)

  implicit def LongFrom(n: LongW) = LongW.LongFrom(n)

  implicit def OptionTo[A](o: Option[A]) = OptionW.OptionTo(o)

  implicit def OptionFrom[A](o: OptionW[A]) = OptionW.OptionFrom(o)

  def multiplication(n: Short) = ShortMultiplication.multiplication(n)

  implicit def ShortMultiplicationFrom(n: ShortMultiplication) = ShortMultiplication.ShortMultiplicationFrom(n)

  implicit def ShortTo(n: Short) = ShortW.ShortTo(n)

  implicit def ShortFrom(n: ShortW) = ShortW.ShortFrom(n)

  implicit def StreamTo[A](as: Stream[A]) = StreamW.StreamTo(as)

  implicit def StreamFrom[A](as: StreamW[A]) = StreamW.StreamFrom(as)

  implicit def StringTo(ss: String) = StringW.StringTo(ss)

  implicit def StringFrom(s: StringW) = StringW.StringFrom(s)

  def zipStream[A](s: Stream[A]) = ZipStream.zip(s)

  implicit def ZipStreamFrom[A](z: ZipStream[A]) = ZipStream.ZipStreamFrom(z)

  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]) = Zipper.zipper(ls, a, rs)

  implicit def StrategyTo[A](s: Strategy[A]) = Strategy.strategyTo(s)

  implicit def StrategyFrom[A](s: (() => A) => () => A) = Strategy.strategyFrom(s)

  // MA

  import MA.ma

  implicit def IdentityMA[A](a: Identity[A]): MA[Identity, A] = ma[Identity](a)

  implicit def ContinuationMA[R, A](a: Continuation[R, A]) = ma[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def NonEmptyListMA[A](a: NonEmptyList[A]) = ma[NonEmptyList](a)

  implicit def StateMA[S, A](a: State[S, A]) = ma[PartialApply1Of2[State, S]#Apply](a)

  implicit def EqualMA[A](a: Equal[A]) = ma[Equal](a)

  implicit def OrderMA[A](a: Order[A]) = ma[Order](a)

  implicit def ShowMA[A](a: Show[A]) = ma[Show](a)

  implicit def ZipStreamMA[A](a: ZipStream[A]) = ma[ZipStream](a)

  implicit def ComemoMA[T, K, V](a: memo.Comemo[T, K, V]) = ma[PartialApply2Of3[memo.Comemo, K, V]#ApplyA].apply[T](a)

  implicit def MetricSpaceStreamMA[A](a: MetricSpace[A]) = ma[MetricSpace](a)

  implicit def Tuple1MA[A](a: Tuple1[A]) = ma[Tuple1](a)

  implicit def Tuple2MA[R, A](a: (R, A)) = ma[PartialApply1Of2[Tuple2, R]#Apply](a)

  implicit def Tuple3MA[R, S, A](a: (R, S, A)) = ma[PartialApply2Of3[Tuple3, R, S]#Apply](a)

  implicit def Tuple4MA[R, S, T, A](a: (R, S, T, A)) = ma[PartialApply3Of4[Tuple4, R, S, T]#Apply](a)

  implicit def Tuple5MA[R, S, T, U, A](a: (R, S, T, U, A)) = ma[PartialApply4Of5[Tuple5, R, S, T, U]#Apply](a)

  implicit def Tuple6MA[R, S, T, U, V, A](a: (R, S, T, U, V, A)) = ma[PartialApply5Of6[Tuple6, R, S, T, U, V]#Apply](a)

  implicit def Tuple7MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, A)) = ma[PartialApply6Of7[Tuple7, R, S, T, U, V, W]#Apply](a)

  implicit def Function0MA[A](a: Function0[A]) = ma[Function0](a)

  implicit def Function1MA[R, A](a: R => A) = ma[PartialApply1Of2[Function1, R]#Apply](a)

  implicit def Function2MA[R, S, A](a: (R, S) => A) = ma[PartialApply2Of3[Function2, R, S]#Apply](a)

  implicit def Function3MA[R, S, T, A](a: (R, S, T) => A) = ma[PartialApply3Of4[Function3, R, S, T]#Apply](a)

  implicit def Function4MA[R, S, T, U, A](a: (R, S, T, U) => A) = ma[PartialApply4Of5[Function4, R, S, T, U]#Apply](a)

  implicit def Function5MA[R, S, T, U, V, A](a: (R, S, T, U, V) => A) = ma[PartialApply5Of6[Function5, R, S, T, U, V]#Apply](a)

  implicit def Function6MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => A) = ma[PartialApply6Of7[Function6, R, S, T, U, V, W]#Apply](a)

  implicit def ListMA[A](a: List[A]) = ma[List](a)

  implicit def StreamMA[A](a: Stream[A]) = ma[Stream](a)

  implicit def OptionMA[A](a: Option[A]) = ma[Option](a)

  implicit def ArrayMA[A](a: Array[A]) = ma[Array](a)

  implicit def EitherLeftMA[X, A](a: Either.LeftProjection[A, X]) = ma[PartialApply1Of2[Either.LeftProjection, X]#Flip](a)

  implicit def EitherRightMA[X, A](a: Either.RightProjection[X, A]) = ma[PartialApply1Of2[Either.RightProjection, X]#Apply](a)

  implicit def ValidationMA[E, A](a: Validation[E, A]) = ma[PartialApply1Of2[Validation, E]#Apply](a)

  implicit def ValidationFailureMA[A, E](a: Validation.FailureProjection[E, A]) = ma[PartialApply1Of2[Validation.FailureProjection, A]#Flip](a)

  implicit def ZipperMA[A](a: Zipper[A]) = ma[Zipper](a)

  implicit def EndoMA[A](a: Endo[A]) = ma[Endo](a)

  implicit def TreeMA[A](a: Tree[A]) = ma[Tree](a)

  implicit def GenMA[A](a: test.Gen[A]) = ma[test.Gen](a)
  /*
  //todo

  implicit def TestableMA[A](a: test.Testable[A]) = ma[test.Testable](a)
    */
  implicit def TreeLocMA[A](a: TreeLoc[A]) = ma[TreeLoc](a)

  import java.util._
  import java.util.concurrent._

  implicit def ArrayListMA[A](a: ArrayList[A]) = ma[ArrayList](a)

  implicit def HashSetMA[A](a: HashSet[A]) = ma[HashSet](a)

  implicit def LinkedHashSetMA[A](a: LinkedHashSet[A]) = ma[LinkedHashSet](a)

  implicit def LinkedListMA[A](a: LinkedList[A]) = ma[LinkedList](a)

  implicit def PriorityQueueMA[A](a: PriorityQueue[A]) = ma[PriorityQueue](a)

  implicit def StackMA[A](a: Stack[A]) = ma[Stack](a)

  implicit def TreeSetMA[A](a: TreeSet[A]) = ma[TreeSet](a)

  implicit def VectorMA[A](a: Vector[A]) = ma[Vector](a)

  implicit def ArrayBlockingQueueMA[A](a: ArrayBlockingQueue[A]) = ma[ArrayBlockingQueue](a)

  implicit def ConcurrentLinkedQueueMA[A](a: ConcurrentLinkedQueue[A]) = ma[ConcurrentLinkedQueue](a)

  implicit def CopyOnWriteArrayListMA[A](a: CopyOnWriteArrayList[A]) = ma[CopyOnWriteArrayList](a)

  implicit def CopyOnWriteArraySetMA[A](a: CopyOnWriteArraySet[A]) = ma[CopyOnWriteArraySet](a)

  implicit def LinkedBlockingQueueMA[A](a: LinkedBlockingQueue[A]) = ma[LinkedBlockingQueue](a)

  implicit def PriorityBlockingQueueMA[A](a: PriorityBlockingQueue[A]) = ma[PriorityBlockingQueue](a)

  implicit def SynchronousQueueMA[A](a: SynchronousQueue[A]) = ma[SynchronousQueue](a)

  // MMA

  import MMA.mma

  implicit def IdentityMMA[A](a: Identity[Identity[A]]) = mma[Identity](a)

  implicit def ContinuationMMA[R, A](a: Continuation[R, Continuation[R, A]]) = mma[PartialApply1Of2[Continuation, R]#Apply](a)

  implicit def NonEmptyListMMA[A](a: NonEmptyList[NonEmptyList[A]]) = mma[NonEmptyList](a)

  implicit def StateMMA[S, A](a: State[S, State[S, A]]) = mma[PartialApply1Of2[State, S]#Apply](a)

  implicit def EqualMMA[A](a: Equal[Equal[A]]) = mma[Equal](a)

  implicit def OrderMMA[A](a: Order[Order[A]]) = mma[Order](a)

  implicit def ShowMMA[A](a: Show[Show[A]]) = mma[Show](a)

  implicit def ZipStreamMMA[A](a: ZipStream[ZipStream[A]]) = mma[ZipStream](a)

  implicit def MetricSpaceMMA[A](a: MetricSpace[MetricSpace[A]]) = mma[MetricSpace](a)

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

  implicit def ValidationMMA[X, A](a: Validation[X, Validation[X, A]]) = mma[PartialApply1Of2[Validation, X]#Apply](a)

  implicit def ValidationFailureMMA[A, X](a: Validation.FailureProjection[Validation.FailureProjection[A, X], X]) = mma[PartialApply1Of2[Validation.FailureProjection, X]#Flip](a)

  implicit def ZipperMMA[A](a: Zipper[Zipper[A]]) = mma[Zipper](a)

  implicit def EndoMMA[A](a: Endo[Endo[A]]) = mma[Endo](a)

  implicit def TreeMMA[A](a: Tree[Tree[A]]) = mma[Tree](a)

  implicit def GenMMA[A](a: test.Gen[test.Gen[A]]) = mma[test.Gen](a)
  /*

  // todo
 
  implicit def TestableMMA[A](a: test.Testable[test.Testable[A]]) = mma[test.Testable](a)
  */
  implicit def TreeLocMMA[A](a: TreeLoc[TreeLoc[A]]) = mma[TreeLoc](a)

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

  // MAB

  import MAB.mab

  implicit def EitherMAB[A, B](a: Either[A, B]): MAB[Either, A, B] = mab[Either](a)

  implicit def Function1MAB[A, B](a: A => B): MAB[Function1, A, B] = mab[Function1](a)

  implicit def Tuple2MAB[A, B](a: (A, B)): MAB[Tuple2, A, B] = mab[Tuple2](a)

  trait KleisliMABApply[M[_]] {
    def apply[A, B](a: Kleisli[M, A, B]): MAB[PartialApplyK[Kleisli, M]#Apply, A, B]
  }

  def KleisliMAB[M[_]] = new KleisliMABApply[M] {
    def apply[A, B](a: Kleisli[M, A, B]): MAB[PartialApplyK[Kleisli, M]#Apply, A, B] = mab[PartialApplyK[Kleisli, M]#Apply](a)
  }

  // todo apply type constructor values to KleisliMAB with implicit

  import java.util._
  import java.util.concurrent._

  implicit def HashMapMAB[A, B](a: HashMap[A, B]): MAB[HashMap, A, B] = mab[HashMap](a)

  implicit def HashtableMAB[A, B](a: Hashtable[A, B]): MAB[Hashtable, A, B] = mab[Hashtable](a)

  implicit def IdentityHashMapMAB[A, B](a: IdentityHashMap[A, B]): MAB[IdentityHashMap, A, B] = mab[IdentityHashMap](a)

  implicit def LinkedHashMapMAB[A, B](a: LinkedHashMap[A, B]): MAB[LinkedHashMap, A, B] = mab[LinkedHashMap](a)

  implicit def TreeMapMAB[A, B](a: TreeMap[A, B]): MAB[TreeMap, A, B] = mab[TreeMap](a)

  implicit def WeakHashMapMAB[A, B](a: WeakHashMap[A, B]): MAB[WeakHashMap, A, B] = mab[WeakHashMap](a)

  implicit def ConcurrentHashMapMAB[A, B](a: ConcurrentHashMap[A, B]): MAB[ConcurrentHashMap, A, B] = mab[ConcurrentHashMap](a)

}