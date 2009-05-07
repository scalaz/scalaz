package scalaz

trait Zero[+Z] {
  val zero: Z
}

object Zero {
  def z[Z](implicit x: Zero[Z]) = x.zero

  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  import S._

  implicit val DigitZero: Zero[Digit] = zero(_0)

  implicit val OrderingZero = zero(EQ)

  implicit val UnitZero = zero(())

  implicit val StringZero = zero("")

  implicit val IntZero = zero(0)

  implicit val IntMultiplicationZero = zero(1 |*|)

  implicit val BooleanConjunctionZero = zero(true |&&|)

  implicit val BooleanZero = zero(false)

  implicit val CharZero = zero(0.toChar)

  implicit val CharMultiplicationZero = zero(1.toChar |*|)

  implicit val ByteZero = zero(0.toByte)

  implicit val ByteMultiplicationZero = zero(1.toByte |*|)

  implicit val LongZero = zero(0L)

  implicit val LongMultiplicationZero = zero(1L |*|)

  implicit val ShortZero = zero(0.toShort)

  implicit val ShortMultiplicationZero = zero(1.toShort |*|)

  implicit val FloatZero = zero(0F)

  implicit val DoubleZero = zero(0D)

  implicit val BigIntegerZero = zero(java.math.BigInteger.valueOf(0))

  implicit val BigIntegerMultiplicationZero = zero(java.math.BigInteger.valueOf(1) |*|)

  implicit val BigIntZero = zero(BigInt(0))

  implicit val BigIntMutliplicationZero = zero(BigInt(1) |*|)

  implicit val NodeSeqZero = zero(xml.NodeSeq.Empty)

  implicit def ZipStreamZero[A] = zero[ZipStream[A]](ZipStream.zip(Stream.empty))

  implicit def ListZero[A] = zero[List[A]](Nil)

  implicit def StreamZero[A] = zero[Stream[A]](Stream.empty)

  implicit def OptionZero[A] = zero[Option[A]](None)

  implicit def ArrayZero[A] = zero(new Array[A](0))

  implicit def EitherLeftZero[A, B](implicit za: Zero[A]) = zero[Either.LeftProjection[A, B]](Left(za.zero).left)

  implicit def EitherRightZero[A, B](implicit za: Zero[A]) = zero[Either.RightProjection[B, A]](Right(za.zero).right)

  implicit def Function1ABZero[A, B](implicit zb: Zero[B]) = zero((_: A) => zb.zero)

  implicit def EndoZero[A] = zero[Endo[A]](identity(_: A))

  implicit def DualZero[A](implicit za: Zero[A]) = zero[Dual[A]](za.zero.dual)

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListZero[A] = zero(new ArrayList[A])

  implicit def JavaHashMapZero[K, V] = zero(new HashMap[K, V])

  implicit def JavaHashSetZero[A] = zero(new HashSet[A])

  implicit def JavaHashtableZero[K, V] = zero(new Hashtable[K, V])

  implicit def JavaIdentityHashMapZero[K, V] = zero(new IdentityHashMap[K, V])

  implicit def JavaLinkedHashMapZero[K, V] = zero(new LinkedHashMap[K, V])

  implicit def JavaLinkedHashSetZero[A] = zero(new LinkedHashSet[A])

  implicit def JavaLinkedListZero[A] = zero(new LinkedList[A])

  implicit def JavaPriorityQueueZero[A] = zero(new PriorityQueue[A])

  implicit def JavaStackZero[A] = zero(new Stack[A])

  implicit def JavaTreeMapZero[K, V] = zero(new TreeMap[K, V])

  implicit def JavaTreeSetZero[A] = zero(new TreeSet[A])

  implicit def JavaVectorZero[A] = zero(new Vector[A])

  implicit def JavaWeakHashMapZero[K, V] = zero(new WeakHashMap[K, V])

  implicit def JavaArrayBlockingQueueZero[A] = zero(new ArrayBlockingQueue[A](0))

  implicit def JavaConcurrentHashMapZero[K, V] = zero(new ConcurrentHashMap[K, V])

  implicit def JavaConcurrentLinkedQueueZero[A] = zero(new ConcurrentLinkedQueue[A])

  implicit def JavaCopyOnWriteArrayListZero[A] = zero(new CopyOnWriteArrayList[A])

  implicit def JavaCopyOnWriteArraySetZero[A] = zero(new CopyOnWriteArraySet[A])

  implicit def JavaLinkedBlockingQueueZero[A] = zero(new LinkedBlockingQueue[A])

  implicit def JavaSynchronousQueueZero[A] = zero(new SynchronousQueue[A])
}
