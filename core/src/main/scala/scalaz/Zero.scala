package scalaz

import collection.generic.CanBuildFrom
import xml.{Elem, Node}
import java.math.BigInteger
import collection.mutable.ArraySeq

trait Zero[A] {
  val zero: A

  def deriving[B](implicit n: ^*^[B, A]): Zero[B] =
    new Zero[B] {
      val zero = n.pack(Zero.this.zero)
    }

}

object Zero extends Zeros

trait Zeros extends ZerosLow {
  def zero[A](a: A): Zero[A] = new Zero[A] {
    val zero = a
  }

  implicit val UnitZero: Zero[Unit] =
    zero(())

  implicit val StringZero: Zero[String] =
    zero("")

  implicit val IntZero: Zero[Int] =
    zero(0)

  implicit val BooleanZero: Zero[Boolean] =
    zero(false)

  implicit val CharZero: Zero[Char] =
    zero(0.toChar)

  implicit val ByteZero: Zero[Byte] =
    zero(0.toByte)

  implicit val LongZero: Zero[Long] =
    zero(0L)

  implicit val ShortZero: Zero[Short] =
    zero(0.toShort)

  implicit val FloatZero: Zero[Float] =
    zero(0F)

  implicit val DoubleZero: Zero[Double] =
    zero(0D)

  implicit def BigIntegerZero: Zero[BigInteger] =
    zero(BigInteger.valueOf(0))

  implicit val BigIntZero: Zero[BigInt] =
    zero(BigInt(0))

  // Not implicit to ensure implicitly[Zero[NodeSeq]].zero === NodeSeqZero.zero
  def NodeZero: Zero[Node] = new Zero[Node] {
    val zero = new Node {
      override def text = null

      override def label = null

      override def child = Nil
    }
  }

  // Not implicit to ensure implicitly[Zero[NodeSeq]].zero === NodeSeqZero.zero
  def ElemZero: Zero[Elem] = new Zero[Elem] {
    val zero = new Elem(null, null, scala.xml.Null, xml.TopScope, Nil: _*)
  }

  implicit def OptionZero[A]: Zero[Option[A]] =
    zero(None)

  implicit def ArrayZero[A: Manifest]: Zero[Array[A]] =
    zero(new Array[A](0))

  implicit def ArraySeqZero[A: Manifest]: Zero[ArraySeq[A]] =
    zero(new ArraySeq[A](0))

  implicit def EitherLeftZero[A, B: Zero]: Zero[Either.LeftProjection[A, B]] =
    zero(Right(implicitly[Zero[B]].zero).left)

  implicit def EitherRightZero[A: Zero, B]: Zero[Either.RightProjection[A, B]] =
    zero(Left(implicitly[Zero[A]].zero).right)

  implicit def EitherZero[A: Zero, B]: Zero[Either[A, B]] =
    zero(Left(implicitly[Zero[A]].zero))

  implicit def StreamZero[A]: Zero[Stream[A]] =
    zero(Stream.Empty)

  implicit def ListZero[A]: Zero[List[A]] =
    zero(Nil)

  implicit def Tuple1Zero[A](implicit za: Zero[A]): Zero[Tuple1[A]] =
    zero(Tuple1(za.zero))

  implicit def Tuple2Zero[A, B](implicit za: Zero[A], zb: Zero[B]): Zero[(A, B)] =
    zero(za.zero, zb.zero)

  implicit def Tuple3Zero[A, B, C](implicit za: Zero[A], zb: Zero[B], zc: Zero[C]): Zero[(A, B, C)] =
    zero(za.zero, zb.zero, zc.zero)

  implicit def Tuple4Zero[A, B, C, D](implicit za: Zero[A], zb: Zero[B], zc: Zero[C], zd: Zero[D]): Zero[(A, B, C, D)] =
    zero(za.zero, zb.zero, zc.zero, zd.zero)

  implicit def Tuple5Zero[A, B, C, D, E](implicit za: Zero[A], zb: Zero[B], zc: Zero[C], zd: Zero[D], ze: Zero[E]): Zero[(A, B, C, D, E)] =
    zero(za.zero, zb.zero, zc.zero, zd.zero, ze.zero)

  implicit def Tuple6Zero[A, B, C, D, E, F](implicit za: Zero[A], zb: Zero[B], zc: Zero[C], zd: Zero[D], ze: Zero[E], zf: Zero[F]): Zero[(A, B, C, D, E, F)] =
    zero(za.zero, zb.zero, zc.zero, zd.zero, ze.zero, zf.zero)

  implicit def Tuple7Zero[A, B, C, D, E, F, G](implicit za: Zero[A], zb: Zero[B], zc: Zero[C], zd: Zero[D], ze: Zero[E], zf: Zero[F], zg: Zero[G]): Zero[(A, B, C, D, E, F, G)] =
    zero(za.zero, zb.zero, zc.zero, zd.zero, ze.zero, zf.zero, zg.zero)

  implicit def Function1Zero[A, B](implicit zb: Zero[B]): Zero[A => B] =
    zero((_: A) => zb.zero)

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListZero[A]: Zero[ArrayList[A]] =
    zero(new ArrayList[A])

  implicit def JavaHashMapZero[K, V]: Zero[HashMap[K, V]] =
    zero(new HashMap[K, V])

  implicit def JavaHashSetZero[A]: Zero[HashSet[A]] =
    zero(new HashSet[A])

  implicit def JavaHashtableZero[K, V]: Zero[Hashtable[K, V]] =
    zero(new Hashtable[K, V])

  implicit def JavaIdentityHashMapZero[K, V] =
    zero(new IdentityHashMap[K, V])

  implicit def JavaLinkedHashMapZero[K, V]: Zero[LinkedHashMap[K, V]] =
    zero(new LinkedHashMap[K, V])

  implicit def JavaLinkedHashSetZero[A]: Zero[LinkedHashSet[A]] =
    zero(new LinkedHashSet[A])

  implicit def JavaLinkedListZero[A]: Zero[LinkedList[A]] =
    zero(new LinkedList[A])

  implicit def JavaPriorityQueueZero[A]: Zero[PriorityQueue[A]] =
    zero(new PriorityQueue[A])

  implicit def JavaStackZero[A]: Zero[Stack[A]] =
    zero(new Stack[A])

  implicit def JavaTreeMapZero[K, V]: Zero[TreeMap[K, V]] =
    zero(new TreeMap[K, V])

  implicit def JavaTreeSetZero[A]: Zero[TreeSet[A]] =
    zero(new TreeSet[A])

  implicit def JavaVectorZero[A]: Zero[Vector[A]] =
    zero(new Vector[A])

  implicit def JavaWeakHashMapZero[K, V]: Zero[WeakHashMap[K, V]] =
    zero(new WeakHashMap[K, V])

  implicit def JavaArrayBlockingQueueZero[A]: Zero[ArrayBlockingQueue[A]] =
    zero(new ArrayBlockingQueue[A](0))

  implicit def JavaConcurrentHashMapZero[K, V]: Zero[ConcurrentHashMap[K, V]] =
    zero(new ConcurrentHashMap[K, V])

  implicit def JavaConcurrentLinkedQueueZero[A]: Zero[ConcurrentLinkedQueue[A]] =
    zero(new ConcurrentLinkedQueue[A])

  implicit def JavaCopyOnWriteArrayListZero[A]: Zero[CopyOnWriteArrayList[A]] =
    zero(new CopyOnWriteArrayList[A])

  implicit def JavaCopyOnWriteArraySetZero[A]: Zero[CopyOnWriteArraySet[A]] =
    zero(new CopyOnWriteArraySet[A])

  implicit def JavaLinkedBlockingQueueZero[A]: Zero[LinkedBlockingQueue[A]] =
    zero(new LinkedBlockingQueue[A])

  implicit def JavaSynchronousQueueZero[A]: Zero[SynchronousQueue[A]] =
    zero(new SynchronousQueue[A])

}

trait ZerosLow {
  implicit def TraversableZero[CC <: Traversable[_]](implicit cbf: CanBuildFrom[Nothing, Nothing, CC]): Zero[CC] =
    new Zero[CC] {
      val zero = cbf.apply.result
    }
}