package scalaz

trait Zero[+Z] {
  val zero: Z
}

object Zero {
  def z[Z](implicit x: Zero[Z]) = x.zero

  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  implicit val OrderingZero = zero(EQ)

  implicit val UnitZero = zero(())

  implicit val StringZero = zero("")

  implicit val IntZero = zero(0)

  implicit val BooleanZero = zero(false)

  implicit val CharZero = zero(0.toChar)

  implicit val ByteZero = zero(0.toByte)

  implicit val LongZero = zero(0L)

  implicit val ShortZero = zero(0.toShort)

  implicit val FloatZero = zero(0F)

  implicit val DoubleZero = zero(0D)

  implicit def ListZero[A] = zero[List[A]](Nil)

  implicit def StreamZero[A] = zero[Stream[A]](Stream.empty)

  implicit def OptionZero[A] = zero[Option[A]](None)

  implicit def ArrayZero[A] = zero(new Array[A](0))

  implicit def EitherLeftZero[A, B](implicit za: Zero[A]) = zero[Either.LeftProjection[A, B]](Left(za.zero).left)

  implicit def EitherRightZero[A, B](implicit za: Zero[A]) = zero[Either.RightProjection[B, A]](Right(za.zero).right)

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
