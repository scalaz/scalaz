package fjs.test

import fjs.F._
import fjs.F2._
import fjs.F3._
import fjs.F4._
import fjs.F5._
import fjs.F6._
import fjs.F7._
import fjs.F8._
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
               
object Coarbitrary {
  private type Arb[A] = fj.test.Arbitrary[A]
  private type Coarb[A] = fj.test.Coarbitrary[A]

  implicit def coarbF[A, B](implicit a: Arb[A], c: Coarb[B]) =
    fj.test.Coarbitrary.coarbF(a, c)

  implicit def coarbF2[A, B, C](implicit aa: Arb[A], ab: Arb[B], c: Coarb[C]) =
    fj.test.Coarbitrary.coarbF2(aa, ab, c)

  implicit def coarbF3[A, B, C, D](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], c: Coarb[D]) =
    fj.test.Coarbitrary.coarbF3(aa, ab, ac, c)

  implicit def coarbF4[A, B, C, D, E](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], c: Coarb[E]) =
    fj.test.Coarbitrary.coarbF4(aa, ab, ac, ad, c)

  implicit def coarbF5[A, B, C, D, E, F$](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], c: Coarb[F$]) =
    fj.test.Coarbitrary.coarbF5(aa, ab, ac, ad, ae, c)

  implicit def coarbF6[A, B, C, D, E, F$, G](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], af: Arb[F$], c: Coarb[G]) =
    fj.test.Coarbitrary.coarbF6(aa, ab, ac, ad, ae, af, c)

  implicit def coarbF7[A, B, C, D, E, F$, G, H](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], af: Arb[F$], ag: Arb[G], c: Coarb[H]) =
    fj.test.Coarbitrary.coarbF7(aa, ab, ac, ad, ae, af, ag, c)

  implicit def coarbF8[A, B, C, D, E, F$, G, H, I](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], af: Arb[F$], ag: Arb[G], ah: Arb[H], c: Coarb[I]) =
    fj.test.Coarbitrary.coarbF8(aa, ab, ac, ad, ae, af, ag, ah, c)

  implicit val coarbBoolean = fj.test.Coarbitrary.coarbBoolean
  implicit val coarbCharacter = fj.test.Coarbitrary.coarbCharacter
  implicit val coarbInteger = fj.test.Coarbitrary.coarbInteger
  implicit val coarbLong = fj.test.Coarbitrary.coarbLong
  implicit val coarbByte = fj.test.Coarbitrary.coarbByte
  implicit val coarbShort = fj.test.Coarbitrary.coarbShort
  implicit val coarbDouble = fj.test.Coarbitrary.coarbDouble
  implicit val coarbFloat = fj.test.Coarbitrary.coarbFloat

  implicit val coarbString = fj.test.Coarbitrary.coarbString
  implicit val coarbStringBuffer = fj.test.Coarbitrary.coarbStringBuffer
  implicit val coarbStringBuilder = fj.test.Coarbitrary.coarbStringBuilder

  implicit def coarbOption[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbOption(ca)

  implicit def coarbEither[A, B](implicit ca: Coarb[A], cb: Coarb[B]) =
    fj.test.Coarbitrary.coarbEither(ca, cb)

  implicit def coarbList[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbList(ca)

  implicit def coarbStream[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbStream(ca)

  implicit def coarbArray[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbArray(ca)

  implicit def coarbArrayList[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbArrayList(ca)

  implicit val coarbBitSet = fj.test.Coarbitrary.coarbBitSet
  implicit val coarbCalendar = fj.test.Coarbitrary.coarbCalendar
  implicit val coarbDate = fj.test.Coarbitrary.coarbDate
            
  implicit def coarbEnumMap[K <: Enum[K], V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbEnumMap(ck, cv)

  implicit def coarbEnumSet[A <: Enum[A]](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbEnumSet(ca)

  implicit val coarbGregorianCalendar = fj.test.Coarbitrary.coarbGregorianCalendar

  implicit def coarbHashMap[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbHashMap(ck, cv)

  implicit def coarbHashSet[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbHashSet(ca)

  implicit def coarbHashtable[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbHashtable(ck, cv)

  implicit def coarbIdentityHashMap[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbIdentityHashMap(ck, cv)

  implicit def coarbLinkedHashMap[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbLinkedHashMap(ck, cv)

  implicit def coarbLinkedHashSet[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbLinkedHashSet(ca)

  implicit def coarbLinkedList[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbLinkedList(ca)

  implicit def coarbPriorityQueue[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbPriorityQueue(ca)

  implicit val coarbProperties = fj.test.Coarbitrary.coarbProperties

  implicit def coarbStack[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbStack(ca)

  implicit def coarbTreeMap[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbTreeMap(ck, cv)

  implicit def coarbTreeSet[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbTreeSet(ca)

  implicit def coarbVector[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbVector(ca)

  implicit def coarbWeakHashMap[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbWeakHashMap(ck, cv)

  implicit def coarbArrayBlockingQueue[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbArrayBlockingQueue(ca)

  implicit def coarbConcurrentHashMap[K, V](implicit ck: Coarb[K], cv: Coarb[V]) =
    fj.test.Coarbitrary.coarbConcurrentHashMap(ck, cv)

  implicit def coarbConcurrentLinkedQueue[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbConcurrentLinkedQueue(ca)

  implicit def coarbCopyOnWriteArrayList[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbCopyOnWriteArrayList(ca)

  implicit def coarbCopyOnWriteArraySet[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbCopyOnWriteArraySet(ca)

  implicit def coarbDelayQueue[A <: java.util.concurrent.Delayed](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbDelayQueue(ca)

  implicit def coarbLinkedBlockingQueue[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbLinkedBlockingQueue(ca)

  implicit def coarbPriorityBlockingQueue[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbPriorityBlockingQueue(ca)

  implicit def coarbSynchronousQueue[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbSynchronousQueue(ca)

  implicit val coarbSQLDate = fj.test.Coarbitrary.coarbSQLDate
  implicit val coarbTime = fj.test.Coarbitrary.coarbTime
  implicit val coarbTimestamp = fj.test.Coarbitrary.coarbTimestamp
  implicit val coarbBigInteger = fj.test.Coarbitrary.coarbBigInteger
  implicit val coarbBigDecimal = fj.test.Coarbitrary.coarbBigDecimal

  implicit def coarbP1[A](implicit ca: Coarb[A]) =
    fj.test.Coarbitrary.coarbP1(ca)

  implicit def coarbP2[A, B](implicit ca: Coarb[A], cb: Coarb[B]) =
    fj.test.Coarbitrary.coarbP2(ca, cb)

  implicit def coarbP3[A, B, C](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C]) =
    fj.test.Coarbitrary.coarbP3(ca, cb, cc)

  implicit def coarbP4[A, B, C, D](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D]) =
    fj.test.Coarbitrary.coarbP4(ca, cb, cc, cd)

  implicit def coarbP5[A, B, C, D, E](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E]) =
    fj.test.Coarbitrary.coarbP5(ca, cb, cc, cd, ce)

  implicit def coarbP6[A, B, C, D, E, F$](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$]) =
    fj.test.Coarbitrary.coarbP6(ca, cb, cc, cd, ce, cf)

  implicit def coarbP7[A, B, C, D, E, F$, G](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], cg: Coarb[G]) =
    fj.test.Coarbitrary.coarbP7(ca, cb, cc, cd, ce, cf, cg)

  implicit def coarbP8[A, B, C, D, E, F$, G, H](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], cg: Coarb[G], ch: Coarb[H]) =
    fj.test.Coarbitrary.coarbP8(ca, cb, cc, cd, ce, cf, cg, ch)

  // BEGIN scala

  implicit val coarbSBoolean: Coarb[Boolean] = coarbBoolean comap (java.lang.Boolean.valueOf(_: Boolean))
  implicit val coarbSByte: Coarb[Byte] = coarbByte comap (java.lang.Byte.valueOf(_: Byte))
  implicit val coarbSChar: Coarb[Char] = coarbCharacter comap (java.lang.Character.valueOf(_: Char))
  implicit val coarbSDouble: Coarb[Double] = coarbDouble comap (java.lang.Double.valueOf(_: Double))
  implicit val coarbSFloat: Coarb[Float] = coarbFloat comap (java.lang.Float.valueOf(_: Float))
  implicit val coarbSInt: Coarb[Int] = coarbInteger comap (java.lang.Integer.valueOf(_: Int))
  implicit val coarbSLong: Coarb[Long] = coarbLong comap (java.lang.Long.valueOf(_: Long))
  implicit val coarbSStringBuilder: Coarb[StringBuilder] = coarbString comap ((_: StringBuilder).toString)

  implicit def coarbSOption[A](implicit a: Coarb[A]): Coarb[Option[A]] = coarbOption(a) comap ((x: Option[A]) => (x: fj.data.Option[A]))
  implicit def coarbSEither[A, B](implicit sa: Coarb[A], sb: Coarb[B]): Coarb[Either[A, B]] = coarbEither(sa, sb) comap ((x: Either[A, B]) => (x: fj.data.Either[A, B]))
  implicit def coarbSList[A](implicit a: Coarb[A]): Coarb[List[A]] = coarbList(a) comap ((x: List[A]) => (x: fj.data.List[A]))
  implicit def coarbSStream[A](implicit a: Coarb[A]): Coarb[Stream[A]] = coarbStream(a) comap ((x: Stream[A]) => (x: fj.data.Stream[A]))
  implicit def coarbSArray[A](implicit a: Coarb[A]): Coarb[Array[A]] = coarbArray(a) comap ((x: Array[A]) => (x: fj.data.Array[A]))

  implicit def coarbSTuple1[A](implicit a: Coarb[A]): Coarb[Tuple1[A]] = coarbP1(a) comap ((x: Tuple1[A]) => (x: fj.P1[A]))
  implicit def coarbSTuple2[A, B](implicit a: Coarb[A], b: Coarb[B]): Coarb[Tuple2[A, B]] = coarbP2(a, b) comap ((x: (A, B)) => (x: fj.P2[A, B]))
  implicit def coarbSTuple3[A, B, C](implicit a: Coarb[A], b: Coarb[B], c: Coarb[C]): Coarb[Tuple3[A, B, C]] = coarbP3(a, b, c) comap ((x: (A, B, C)) => (x: fj.P3[A, B, C]))
  implicit def coarbSTuple4[A, B, C, D](implicit a: Coarb[A], b: Coarb[B], c: Coarb[C], d: Coarb[D]): Coarb[Tuple4[A, B, C, D]] = coarbP4(a, b, c, d) comap ((x: (A, B, C, D)) => (x: fj.P4[A, B, C, D]))
  implicit def coarbSTuple5[A, B, C, D, E](implicit a: Coarb[A], b: Coarb[B], c: Coarb[C], d: Coarb[D], e: Coarb[E]): Coarb[Tuple5[A, B, C, D, E]] = coarbP5(a, b, c, d, e) comap ((x: (A, B, C, D, E)) => (x: fj.P5[A, B, C, D, E]))
  implicit def coarbSTuple6[A, B, C, D, E, F$](implicit a: Coarb[A], b: Coarb[B], c: Coarb[C], d: Coarb[D], e: Coarb[E], f: Coarb[F$]): Coarb[Tuple6[A, B, C, D, E, F$]] = coarbP6(a, b, c, d, e, f) comap ((x: (A, B, C, D, E, F$)) => (x: fj.P6[A, B, C, D, E, F$]))
  implicit def coarbSTuple7[A, B, C, D, E, F$, G](implicit a: Coarb[A], b: Coarb[B], c: Coarb[C], d: Coarb[D], e: Coarb[E], f: Coarb[F$], g: Coarb[G]): Coarb[Tuple7[A, B, C, D, E, F$, G]] = coarbP7(a, b, c, d, e, f, g) comap ((x: (A, B, C, D, E, F$, G)) => (x: fj.P7[A, B, C, D, E, F$, G]))
  implicit def coarbSTuple8[A, B, C, D, E, F$, G, H](implicit a: Coarb[A], b: Coarb[B], c: Coarb[C], d: Coarb[D], e: Coarb[E], f: Coarb[F$], g: Coarb[G], h: Coarb[H]): Coarb[Tuple8[A, B, C, D, E, F$, G, H]] = coarbP8(a, b, c, d, e, f, g, h) comap ((x: (A, B, C, D, E, F$, G, H)) => (x: fj.P8[A, B, C, D, E, F$, G, H]))

  implicit def coarbSF[A, B](implicit a: Arb[A], c: Coarb[B]): Coarb[(A => B)] =
    coarbF(a, c) comap ((x: A => B) => (x: fj.F[A, B]))

  implicit def coarbSF2[A, B, C](implicit aa: Arb[A], ab: Arb[B], c: Coarb[C]): Coarb[((A, B) => C)] =
    coarbF2(aa, ab, c) comap ((x: (A, B) => C) => (x: fj.F2[A, B, C]))

  implicit def coarbSF3[A, B, C, D](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], c: Coarb[D]): Coarb[((A, B, C) => D)] =
    coarbF3(aa, ab, ac, c) comap ((x: (A, B, C) => D) => (x: fj.F3[A, B, C, D]))

  implicit def coarbSF4[A, B, C, D, E](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], c: Coarb[E]): Coarb[((A, B, C, D) => E)] =
    coarbF4(aa, ab, ac, ad, c) comap ((x: (A, B, C, D) => E) => (x: fj.F4[A, B, C, D, E]))

  implicit def coarbSF5[A, B, C, D, E, F$](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], c: Coarb[F$]): Coarb[((A, B, C, D, E) => F$)] =
    coarbF5(aa, ab, ac, ad, ae, c) comap ((x: (A, B, C, D, E) => F$) => (x: fj.F5[A, B, C, D, E, F$]))

  implicit def coarbSF6[A, B, C, D, E, F$, G](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], af: Arb[F$], c: Coarb[G]): Coarb[((A, B, C, D, E, F$) => G)] =
    coarbF6(aa, ab, ac, ad, ae, af, c) comap ((x: (A, B, C, D, E, F$) => G) => (x: fj.F6[A, B, C, D, E, F$, G]))

  implicit def coarbSF7[A, B, C, D, E, F$, G, H](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], af: Arb[F$], ag: Arb[G], c: Coarb[H]): Coarb[((A, B, C, D, E, F$, G) => H)] =
    coarbF7(aa, ab, ac, ad, ae, af, ag, c) comap ((x: (A, B, C, D, E, F$, G) => H) => (x: fj.F7[A, B, C, D, E, F$, G, H]))

  implicit def coarbSF8[A, B, C, D, E, F$, G, H, I](implicit aa: Arb[A], ab: Arb[B], ac: Arb[C], ad: Arb[D], ae: Arb[E], af: Arb[F$], ag: Arb[G], ah: Arb[H], c: Coarb[I]): Coarb[((A, B, C, D, E, F$, G, H) => I)] =
    coarbF8(aa, ab, ac, ad, ae, af, ag, ah, c) comap ((x: (A, B, C, D, E, F$, G, H) => I) => (x: fj.F8[A, B, C, D, E, F$, G, H, I]))

  // END scala
}
