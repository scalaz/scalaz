package fjs.test

import fjs.F._
import fjs.data.Option._
import fjs.data.Either._
import fjs.data.List._
import fjs.data.Stream._
import fjs.data.Array._
import fjs.P1._
import fjs.P2._
import fjs.P3._
import fjs.P4._
import fjs.P5._
import fjs.P6._
import fjs.P7._
import fjs.P8._

object Shrink {
  private type Shr[A] = fj.test.Shrink[A]
  
  implicit def shrink[A](f: A => fj.data.Stream[A]): Shr[A] =
    fj.test.Shrink.shrink(f)

  implicit val shrinkLong = fj.test.Shrink.shrinkLong
  implicit val shrinkCharacter = fj.test.Shrink.shrinkCharacter
  implicit val shrinkBoolean = fj.test.Shrink.shrinkBoolean
  implicit val shrinkInteger = fj.test.Shrink.shrinkInteger
  implicit val shrinkByte = fj.test.Shrink.shrinkByte
  implicit val shrinkShort = fj.test.Shrink.shrinkShort
  implicit val shrinkDouble = fj.test.Shrink.shrinkDouble
  implicit val shrinkFloat = fj.test.Shrink.shrinkFloat

  implicit val shrinkString = fj.test.Shrink.shrinkString
  implicit val shrinkStringBuffer = fj.test.Shrink.shrinkStringBuffer
  implicit val shrinkStringBuilder = fj.test.Shrink.shrinkStringBuilder

  implicit def shrinkOption[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkOption(sa)

  implicit def shrinkEither[A, B](implicit sa: Shr[A], sb: Shr[B]) =
    fj.test.Shrink.shrinkEither(sa, sb)

  implicit def shrinkList[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkList(sa)

  implicit def shrinkStream[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkStream(sa)

  implicit def shrinkArray[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkArray(sa)

  implicit def shrinkArrayList[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkArrayList(sa)

  implicit val shrinkBitSet = fj.test.Shrink.shrinkBitSet
  implicit val shrinkCalendar = fj.test.Shrink.shrinkCalendar
  implicit val shrinkDate = fj.test.Shrink.shrinkDate

  implicit def shrinkEnumMap[K <: Enum[K], V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkEnumMap(sk, sv)

  implicit def shrinkEnumSet[A <: Enum[A]](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkEnumSet(sa)

  implicit val shrinkGregorianCalendar = fj.test.Shrink.shrinkGregorianCalendar

  implicit def shrinkHashMap[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkHashMap(sk, sv)

  implicit def shrinkHashSet[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkHashSet(sa)

  implicit def shrinkHashtable[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkHashtable(sk, sv)

  implicit def shrinkIdentityHashMap[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkIdentityHashMap(sk, sv)

  implicit def shrinkLinkedHashMap[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkLinkedHashMap(sk, sv)

  implicit def shrinkLinkedHashSet[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkLinkedHashSet(sa)

  implicit def shrinkLinkedList[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkLinkedList(sa)

  implicit def shrinkPriorityQueue[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkPriorityQueue(sa)

  implicit val shrinkProperties = fj.test.Shrink.shrinkProperties

  implicit def shrinkStack[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkStack(sa)

  implicit def shrinkTreeMap[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkTreeMap(sk, sv)

  implicit def shrinkTreeSet[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkTreeSet(sa)

  implicit def shrinkVector[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkVector(sa)

  implicit def shrinkWeakHashMap[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkWeakHashMap(sk, sv)

  implicit def shrinkArrayBlockingQueue[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkArrayBlockingQueue(sa)

  implicit def shrinkConcurrentHashMap[K, V](implicit sk: Shr[K], sv: Shr[V]) =
    fj.test.Shrink.shrinkConcurrentHashMap(sk, sv)

  implicit def shrinkConcurrentLinkedQueue[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkConcurrentLinkedQueue(sa)

  implicit def shrinkCopyOnWriteArrayList[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkCopyOnWriteArrayList(sa)

  implicit def shrinkCopyOnWriteArraySet[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkCopyOnWriteArraySet(sa)

  implicit def shrinkDelayQueue[A <: java.util.concurrent.Delayed](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkDelayQueue(sa)

  implicit def shrinkLinkedBlockingQueue[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkLinkedBlockingQueue(sa)

  implicit def shrinkPriorityBlockingQueue[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkPriorityBlockingQueue(sa)

  implicit def shrinkSynchronousQueue[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkSynchronousQueue(sa)

  implicit val shrinkSQLDate = fj.test.Shrink.shrinkSQLDate
  implicit val shrinkTime = fj.test.Shrink.shrinkTime
  implicit val shrinkTimestamp = fj.test.Shrink.shrinkTimestamp
  implicit val shrinkBigInteger = fj.test.Shrink.shrinkBigInteger
  implicit val shrinkBigDecimal = fj.test.Shrink.shrinkBigDecimal

  implicit def shrinkP1[A](implicit sa: Shr[A]) =
    fj.test.Shrink.shrinkP1(sa)

  implicit def shrinkP2[A, B](implicit sa: Shr[A], sb: Shr[B]) =
    fj.test.Shrink.shrinkP2(sa, sb)

  implicit def shrinkP3[A, B, C](implicit sa: Shr[A], sb: Shr[B], sc: Shr[C]) =
    fj.test.Shrink.shrinkP3(sa, sb, sc)

  implicit def shrinkP4[A, B, C, D](implicit sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D]) =
    fj.test.Shrink.shrinkP4(sa, sb, sc, sd)

  implicit def shrinkP5[A, B, C, D, E](implicit sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E]) =
    fj.test.Shrink.shrinkP5(sa, sb, sc, sd, se)

  implicit def shrinkP6[A, B, C, D, E, F$](implicit sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E], sf: Shr[F$]) =
    fj.test.Shrink.shrinkP6(sa, sb, sc, sd, se, sf)

  implicit def shrinkP7[A, B, C, D, E, F$, G](implicit sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E], sf: Shr[F$], sg: Shr[G]) =
    fj.test.Shrink.shrinkP7(sa, sb, sc, sd, se, sf, sg)

  implicit def shrinkP8[A, B, C, D, E, F$, G, H](implicit sa: Shr[A], sb: Shr[B], sc: Shr[C], sd: Shr[D], se: Shr[E], sf: Shr[F$], sg: Shr[G], sh: Shr[H]) =
    fj.test.Shrink.shrinkP8(sa, sb, sc, sd, se, sf, sg, sh)

  // BEGIN scala

  implicit val shrinkSBoolean: Shr[Boolean] = shrinkBoolean map ((_: java.lang.Boolean).booleanValue, java.lang.Boolean.valueOf(_: Boolean))
  implicit val shrinkSByte: Shr[Byte] = shrinkByte map ((_: java.lang.Byte).byteValue, java.lang.Byte.valueOf(_: Byte))
  implicit val shrinkSChar: Shr[Char] = shrinkCharacter map ((_: java.lang.Character).charValue, java.lang.Character.valueOf(_: Char))
  implicit val shrinkSDouble: Shr[Double] = shrinkDouble map ((_: java.lang.Double).doubleValue, java.lang.Double.valueOf(_: Double))
  implicit val shrinkSFloat: Shr[Float] = shrinkFloat map ((_: java.lang.Float).floatValue, java.lang.Float.valueOf(_: Float))
  implicit val shrinkSInt: Shr[Int] = shrinkInteger map ((_: java.lang.Integer).intValue, java.lang.Integer.valueOf(_: Int))
  implicit val shrinkSLong: Shr[Long] = shrinkLong map ((_: java.lang.Long).longValue, java.lang.Long.valueOf(_: Long))
  implicit val shrinkSStringBuilder: Shr[StringBuilder] = shrinkString map (new StringBuilder(_: java.lang.String), (_: StringBuilder).toString)

  implicit def shrinkSOption[A](implicit a: Shr[A]): Shr[Option[A]] = shrinkOption(a) map ((x: fj.data.Option[A]) => (x: Option[A]), (x: Option[A]) => (x: fj.data.Option[A]))
  implicit def shrinkSEither[A, B](implicit sa: Shr[A], sb: Shr[B]): Shr[Either[A, B]] = shrinkEither(sa, sb) map ((x: fj.data.Either[A, B]) => (x: Either[A, B]), (x: Either[A, B]) => (x: fj.data.Either[A, B]))
  implicit def shrinkSList[A](implicit a: Shr[A]): Shr[List[A]] = shrinkList(a) map ((x: fj.data.List[A]) => (x: List[A]), (x: List[A]) => (x: fj.data.List[A]))
  implicit def shrinkSStream[A](implicit a: Shr[A]): Shr[Stream[A]] = shrinkStream(a) map ((x: fj.data.Stream[A]) => (x: Stream[A]), (x: Stream[A]) => (x: fj.data.Stream[A]))
  implicit def shrinkSArray[A](implicit a: Shr[A]): Shr[Array[A]] = shrinkArray(a) map ((x: fj.data.Array[A]) => (x: Array[A]), (x: Array[A]) => (x: fj.data.Array[A]))

  implicit def shrinkSTuple1[A](implicit a: Shr[A]): Shr[Tuple1[A]] = shrinkP1(a) map ((x: fj.P1[A]) => (x: Tuple1[A]), (x: Tuple1[A]) => (x: fj.P1[A]))
  implicit def shrinkSTuple2[A, B](implicit a: Shr[A], b: Shr[B]): Shr[Tuple2[A, B]] = shrinkP2(a, b) map ((x: fj.P2[A, B]) => (x: (A, B)), (x: (A, B)) => (x: fj.P2[A, B]))
  implicit def shrinkSTuple3[A, B, C](implicit a: Shr[A], b: Shr[B], c: Shr[C]): Shr[Tuple3[A, B, C]] = shrinkP3(a, b, c) map ((x: fj.P3[A, B, C]) => (x: (A, B, C)), (x: (A, B, C)) => (x: fj.P3[A, B, C]))
  implicit def shrinkSTuple4[A, B, C, D](implicit a: Shr[A], b: Shr[B], c: Shr[C], d: Shr[D]): Shr[Tuple4[A, B, C, D]] = shrinkP4(a, b, c, d) map ((x: fj.P4[A, B, C, D]) => (x: (A, B, C, D)), (x: (A, B, C, D)) => (x: fj.P4[A, B, C, D]))
  implicit def shrinkSTuple5[A, B, C, D, E](implicit a: Shr[A], b: Shr[B], c: Shr[C], d: Shr[D], e: Shr[E]): Shr[Tuple5[A, B, C, D, E]] = shrinkP5(a, b, c, d, e) map ((x: fj.P5[A, B, C, D, E]) => (x: (A, B, C, D, E)), (x: (A, B, C, D, E)) => (x: fj.P5[A, B, C, D, E]))
  implicit def shrinkSTuple6[A, B, C, D, E, F$](implicit a: Shr[A], b: Shr[B], c: Shr[C], d: Shr[D], e: Shr[E], f: Shr[F$]): Shr[Tuple6[A, B, C, D, E, F$]] = shrinkP6(a, b, c, d, e, f) map ((x: fj.P6[A, B, C, D, E, F$]) => (x: (A, B, C, D, E, F$)), (x: (A, B, C, D, E, F$)) => (x: fj.P6[A, B, C, D, E, F$]))
  implicit def shrinkSTuple7[A, B, C, D, E, F$, G](implicit a: Shr[A], b: Shr[B], c: Shr[C], d: Shr[D], e: Shr[E], f: Shr[F$], g: Shr[G]): Shr[Tuple7[A, B, C, D, E, F$, G]] = shrinkP7(a, b, c, d, e, f, g) map ((x: fj.P7[A, B, C, D, E, F$, G]) => (x: (A, B, C, D, E, F$, G)), (x: (A, B, C, D, E, F$, G)) => (x: fj.P7[A, B, C, D, E, F$, G]))
  implicit def shrinkSTuple8[A, B, C, D, E, F$, G, H](implicit a: Shr[A], b: Shr[B], c: Shr[C], d: Shr[D], e: Shr[E], f: Shr[F$], g: Shr[G], h: Shr[H]): Shr[Tuple8[A, B, C, D, E, F$, G, H]] = shrinkP8(a, b, c, d, e, f, g, h) map ((x: fj.P8[A, B, C, D, E, F$, G, H]) => (x: (A, B, C, D, E, F$, G, H)), (x: (A, B, C, D, E, F$, G, H)) => (x: fj.P8[A, B, C, D, E, F$, G, H]))

  // END scala
}
