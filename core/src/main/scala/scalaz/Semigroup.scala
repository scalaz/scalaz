package scalaz

import java.math.BigInteger
import xml.NodeSeq
import collection.mutable.ArraySeq

trait Semigroup[A] {
  def append(a1: A, a2: => A): A

  def deriving[B](implicit n: ^*^[B, A]): Semigroup[B] =
    new Semigroup[B] {
      def append(b1: B, b2: => B) =
        n.pack(Semigroup.this.append(n.unpack(b1), n.unpack(b2)))
    }

}

object Semigroup extends Semigroups

trait Semigroups extends SemigroupsLow {
  def semigroup[A](k: A => (=> A) => A): Semigroup[A] = new Semigroup[A] {
    def append(a1: A, a2: => A) =
      k(a1)(a2)
  }

  implicit val UnitSemigroup: Semigroup[Unit] =
    semigroup(_ => _ => ())

  implicit val StringSemigroup: Semigroup[String] =
    semigroup(a1 => a2 => a1 + a2)

  implicit val IntSemigroup: Semigroup[Int] =
    semigroup(a => b => a + b)

  implicit val BooleanSemigroup: Semigroup[Boolean] =
    semigroup(a => b => a || b)

  implicit val CharSemigroup: Semigroup[Char] =
    semigroup(a => b => (a + b).toChar)

  implicit val ByteSemigroup: Semigroup[Byte] =
    semigroup(a => b => (a + b).toByte)

  implicit val LongSemigroup: Semigroup[Long] =
    semigroup(a => b => (a + b).toLong)

  implicit val ShortSemigroup: Semigroup[Short] =
    semigroup(a => b => (a + b).toShort)

  implicit val FloatSemigroup: Semigroup[Float] =
    semigroup(a => b => (a + b).toFloat)

  implicit val DoubleSemigroup: Semigroup[Double] =
    semigroup(a => b => (a + b).toDouble)

  implicit val BigIntegerSemigroup: Semigroup[BigInteger] =
    semigroup(a => b => a add b)

  implicit val BigIntSemigroup: Semigroup[BigInt] =
    semigroup(a => b => a + b)

  implicit def NodeSeqSemigroup: Semigroup[NodeSeq] =
    semigroup(a => b => a ++ b)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] =
    semigroup(a1 => a2 => a1 #::: a2)

  implicit def OptionSemigroup[A: Semigroup]: Semigroup[Option[A]] =
    semigroup(a => b =>
      (a, b) match {
        case (Some(va), Some(vb)) => Some(implicitly[Semigroup[A]].append(va, vb))
        case (Some(va), None) => Some(va)
        case (None, Some(vb)) => Some(vb)
        case (None, None) => None
      }
    )

  implicit def ArraySemigroup[A: Manifest]: Semigroup[Array[A]] =
    semigroup(a => b => Array.concat(a, b))

  implicit def ArraySeqSemigroup[A]: Semigroup[ArraySeq[A]] =
    semigroup(a => b => a ++ b)

  implicit def EitherLeftSemigroup[A, B]: Semigroup[Either.LeftProjection[A, B]] =
    semigroup(a => b => if (a.e.isLeft) a else b)

  implicit def EitherRightSemigroup[A, B]: Semigroup[Either.RightProjection[B, A]] =
    semigroup(a => b => if (a.e.isRight) a else b)

  implicit def EitherSemigroup[A, B]: Semigroup[Either[B, A]] =
    semigroup(a => b => if (a.isRight) a else b)

  implicit def ListSemigroup[A]: Semigroup[List[A]] =
    semigroup(a1 => a2 => a1 ::: a2)

  implicit def Tuple2Semigroup[A, B](implicit sa: Semigroup[A], sb: Semigroup[B]): Semigroup[(A, B)] =
    semigroup(a1 => a2 =>
      (sa.append(a1._1, a2._1), sb.append(a1._2, a2._2)))

  implicit def Tuple3Semigroup[A, B, C](implicit sa: Semigroup[A], sb: Semigroup[B], sc: Semigroup[C]): Semigroup[(A, B, C)] =
    semigroup(a1 => a2 =>
      (sa.append(a1._1, a2._1), sb.append(a1._2, a2._2), sc.append(a1._3, a2._3)))

  implicit def Function1Semigroup[A, B](implicit sb: Semigroup[B]): Semigroup[A => B] =
    semigroup(a1 => a2 =>
      a => sb.append(a1(a), a2.apply(a)))

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListSemigroup[A]: Semigroup[ArrayList[A]] = semigroup(a => b => {
    val k = a.clone.asInstanceOf[ArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedListSemigroup[A]: Semigroup[LinkedList[A]] = semigroup(a => b => {
    val k = a.clone.asInstanceOf[LinkedList[A]]
    k addAll b
    k
  })

  implicit def JavaPriorityQueueSemigroup[A]: Semigroup[PriorityQueue[A]] = semigroup(a => b => {
    val k = new PriorityQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaStackSemigroup[A]: Semigroup[Stack[A]] = semigroup(a => b => {
    val k = a.clone.asInstanceOf[Stack[A]]
    k addAll b
    k
  })

  implicit def JavaVectorSemigroup[A]: Semigroup[Vector[A]] = semigroup(a => b => {
    val k = a.clone.asInstanceOf[Vector[A]]
    k addAll b
    k
  })

  implicit def JavaArrayBlockingQueueSemigroup[A]: Semigroup[ArrayBlockingQueue[A]] = semigroup(a => b => {
    val k = new ArrayBlockingQueue[A](a.remainingCapacity + b.remainingCapacity)
    k addAll a
    k addAll b
    k
  })

  implicit def JavaConcurrentLinkedQueueSemigroup[A]: Semigroup[ConcurrentLinkedQueue[A]] = semigroup(a => b => {
    val k = new ConcurrentLinkedQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaCopyOnWriteArrayListSemigroup[A]: Semigroup[CopyOnWriteArrayList[A]] = semigroup(a => b => {
    val k = a.clone.asInstanceOf[CopyOnWriteArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedBlockingQueueSemigroup[A]: Semigroup[LinkedBlockingQueue[A]] = semigroup(a => b => {
    val k = new LinkedBlockingQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaSynchronousQueueSemigroup[A]: Semigroup[SynchronousQueue[A]] = semigroup(a => b => {
    val k = new SynchronousQueue[A]
    k addAll a
    k addAll b
    k
  })

}

trait SemigroupsLow {
  implicit def TraversableSemigroup[X, CC[Y] <: collection.TraversableLike[Y, CC[Y]] : CanBuildAnySelf]: Semigroup[CC[X]] = new Semigroup[CC[X]] {
    def append(s1: CC[X], s2: => CC[X]): CC[X] = {
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[X, X]
      s1 ++ s2
    }
  }
}