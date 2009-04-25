package scalaz

object S {
  import java.math.BigInteger

  implicit def id[A](x: A) = Identity.id(x)
  
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

  def zip[A](s: Stream[A]) = ZipStream.zip(s)

  implicit def ZipStreamFrom[A](z: ZipStream[A]) = ZipStream.ZipStreamFrom(z)

  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]) = Zipper.zipper(ls, a, rs)

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

  implicit def ZipperMA[A](a: Zipper[A]) = ma[Zipper](a)

  implicit def EndoMA[A](a: Endo[A]) = ma[Endo](a)

  implicit def TreeMA[A](a: Tree[A]) = ma[Tree](a)

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

  implicit def ComemoMA[T, K, V](a: memo.Comemo[T, K, V]) = ma[PartialApply2Of3[memo.Comemo, K, V]#ApplyA].apply[T](a)

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