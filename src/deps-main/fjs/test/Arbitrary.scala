package fjs.test

import fjs.F._

// This type is invariant in A to make it a useful implicit value.
// See http://lampsvn.epfl.ch/trac/scala/ticket/591
// Ideally, it would be covariant.
sealed abstract class Arbitrary[A] {
  val gen: Gen[A]

  def >[B](f: A => B): Arbitrary[B] = Arbitrary.arb(gen map f)

  import Arbitrary.arb
  
  def >>=[B](f: A => Arbitrary[B]): Arbitrary[B] =
     arb(gen >>= (f andThen (_.gen)))

  def apply[B](a: Arbitrary[A => B]): Arbitrary[B] = arb(gen(a.gen))
  
  def >>=[B, C](ab: Arbitrary[B], f: A => B => C): Arbitrary[C] =
    arb(gen >>= (ab.gen, f))

  def >>=[B, C, D](ab: Arbitrary[B], ac: Arbitrary[C], f: A => B => C => D): Arbitrary[D] =
    arb(gen >>= (ab.gen, ac.gen, f))

  def >>=[B, C, D, E](ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], f: A => B => C => D => E): Arbitrary[E] =
    arb(gen >>= (ab.gen, ac.gen, ad.gen, f))

  def >>=[B, C, D, E, F$](ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], f: A => B => C => D => E => F$): Arbitrary[F$] =
    arb(gen >>= (ab.gen, ac.gen, ad.gen, ae.gen, f))

  def >>=[B, C, D, E, F$, G](ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], f: A => B => C => D => E => F$ => G): Arbitrary[G] =
    arb(gen >>= (ab.gen, ac.gen, ad.gen, ae.gen, af.gen, f))

  def >>=[B, C, D, E, F$, G, H](ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], f: A => B => C => D => E => F$ => G => H): Arbitrary[H] =
    arb(gen >>= (ab.gen, ac.gen, ad.gen, ae.gen, af.gen, ag.gen, f))

  def >>=[B, C, D, E, F$, G, H, I](ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], ah: Arbitrary[H], f: A => B => C => D => E => F$ => G => H => I): Arbitrary[I] =
    arb(gen >>= (ab.gen, ac.gen, ad.gen, ae.gen, af.gen, ag.gen, ah.gen, f))
  
  def flatMap[B](f: A => Arbitrary[B]): Arbitrary[B] = >>= (f)
  
  def map[B](f: A => B): Arbitrary[B] = > (f)

  def filter(p: A => Boolean): Arbitrary[A] = arb(gen filter (p(_: A): Boolean))

  def <*>[B](a:Arbitrary[A=>B]):Arbitrary[B] = apply(a)

}

import fj.{F, F2, F3, F4, F5, F6, F7, F8}
import fj.{P1, P2, P3, P4, P5, P6, P7, P8}
import java.util._
import java.util.concurrent._ 
import java.sql.{Time, Timestamp}
import java.math.{BigDecimal, BigInteger}

object Arbitrary {  
  private type Coarb[A] = fj.test.Coarbitrary[A]

  def arb[A](g: Gen[A]): Arbitrary[A] = new Arbitrary[A] {
    val gen = g
  }

  def arbitrary[A](implicit a: Arbitrary[A]) = a

  implicit def SArbitrary_Arbitrary[A](a: Arbitrary[A]): fj.test.Arbitrary[A] = fj.test.Arbitrary.arbitrary(a.gen)

  implicit def Arbitrary_SArbitrary[A](a: fj.test.Arbitrary[A]): Arbitrary[A] = arb(a.gen)

  implicit def arbF[A, B](implicit c: Coarb[A], a: Arbitrary[B]): Arbitrary[F[A, B]] =
    fj.test.Arbitrary.arbF(c, a)

  implicit def arbF2[A, B, C](implicit ca: Coarb[A], cb: Coarb[B], a: Arbitrary[C]): Arbitrary[F2[A, B, C]] =
    fj.test.Arbitrary.arbF2(ca, cb, a)

  implicit def arbF3[A, B, C, D](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], a: Arbitrary[D]): Arbitrary[F3[A, B, C, D]] =
    fj.test.Arbitrary.arbF3(ca, cb, cc, a)

  implicit def arbF4[A, B, C, D, E](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], a: Arbitrary[E]): Arbitrary[F4[A, B, C, D, E]] =
    fj.test.Arbitrary.arbF4(ca, cb, cc, cd, a)

  implicit def arbF5[A, B, C, D, E, F$](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], a: Arbitrary[F$]): Arbitrary[F5[A, B, C, D, E, F$]] =
    fj.test.Arbitrary.arbF5(ca, cb, cc, cd, ce, a)

  implicit def arbF6[A, B, C, D, E, F$, G](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], a: Arbitrary[G]): Arbitrary[F6[A, B, C, D, E, F$, G]] =
    fj.test.Arbitrary.arbF6(ca, cb, cc, cd, ce, cf, a)

  implicit def arbF7[A, B, C, D, E, F$, G, H](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], cg: Coarb[G], a: Arbitrary[H]): Arbitrary[F7[A, B, C, D, E, F$, G, H]] =
    fj.test.Arbitrary.arbF7(ca, cb, cc, cd, ce, cf, cg, a)

  implicit def arbF8[A, B, C, D, E, F$, G, H, I](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], cg: Coarb[G], ch: Coarb[H], a: Arbitrary[I]): Arbitrary[F8[A, B, C, D, E, F$, G, H, I]] =
    fj.test.Arbitrary.arbF8(ca, cb, cc, cd, ce, cf, cg, ch, a)
  
  object FInvariant {
    implicit def arbFInvariant[A, B](implicit a: Arbitrary[B]): Arbitrary[F[A, B]] =
      fj.test.Arbitrary.arbFInvariant(a)

    implicit def arbF2Invariant[A, B, C](implicit a: Arbitrary[C]): Arbitrary[F2[A, B, C]] =
      fj.test.Arbitrary.arbF2Invariant(a)

    implicit def arbF3Invariant[A, B, C, D](implicit a: Arbitrary[D]): Arbitrary[F3[A, B, C, D]] =
      fj.test.Arbitrary.arbF3Invariant(a)

    implicit def arbF4Invariant[A, B, C, D, E](implicit a: Arbitrary[E]): Arbitrary[F4[A, B, C, D, E]] =
      fj.test.Arbitrary.arbF4Invariant(a)

    implicit def arbF5Invariant[A, B, C, D, E, F$](implicit a: Arbitrary[F$]): Arbitrary[F5[A, B, C, D, E, F$]] =
      fj.test.Arbitrary.arbF5Invariant(a)

    implicit def arbF6Invariant[A, B, C, D, E, F$, G](implicit a: Arbitrary[G]): Arbitrary[F6[A, B, C, D, E, F$, G]] =
      fj.test.Arbitrary.arbF6Invariant(a)

    implicit def arbF7Invariant[A, B, C, D, E, F$, G, H](implicit a: Arbitrary[H]): Arbitrary[F7[A, B, C, D, E, F$, G, H]] =
      fj.test.Arbitrary.arbF7Invariant(a)

    implicit def arbF8Invariant[A, B, C, D, E, F$, G, H, I](implicit a: Arbitrary[I]): Arbitrary[F8[A, B, C, D, E, F$, G, H, I]] =
      fj.test.Arbitrary.arbF8Invariant(a)
  }

  implicit val arbBoolean: Arbitrary[java.lang.Boolean] = fj.test.Arbitrary.arbBoolean
  implicit val arbCharacter: Arbitrary[java.lang.Character] = fj.test.Arbitrary.arbCharacter
  implicit val arbInteger: Arbitrary[java.lang.Integer] = fj.test.Arbitrary.arbInteger
  implicit val arbLong: Arbitrary[java.lang.Long] = fj.test.Arbitrary.arbLong
  implicit val arbByte: Arbitrary[java.lang.Byte] = fj.test.Arbitrary.arbByte
  implicit val arbShort: Arbitrary[java.lang.Short] = fj.test.Arbitrary.arbShort
  implicit val arbDouble: Arbitrary[java.lang.Double] = fj.test.Arbitrary.arbDouble
  implicit val arbFloat: Arbitrary[java.lang.Float] = fj.test.Arbitrary.arbFloat

  object Boundaries {
    implicit val arbCharacterBoundaries: Arbitrary[java.lang.Character] = fj.test.Arbitrary.arbCharacterBoundaries
    implicit val arbIntegerBoundaries: Arbitrary[java.lang.Integer] = fj.test.Arbitrary.arbIntegerBoundaries
    implicit val arbLongBoundaries: Arbitrary[java.lang.Long] = fj.test.Arbitrary.arbLongBoundaries
    implicit val arbByteBoundaries: Arbitrary[java.lang.Byte] = fj.test.Arbitrary.arbByteBoundaries
    implicit val arbShortBoundaries: Arbitrary[java.lang.Short] = fj.test.Arbitrary.arbShortBoundaries
    implicit val arbDoubleBoundaries: Arbitrary[java.lang.Double] = fj.test.Arbitrary.arbDoubleBoundaries
    implicit val arbFloatBoundaries: Arbitrary[java.lang.Float] = fj.test.Arbitrary.arbFloatBoundaries
  }
  
  implicit val arbString: Arbitrary[String] = fj.test.Arbitrary.arbString
  implicit val arbUSASCIIString: Arbitrary[String] = fj.test.Arbitrary.arbUSASCIIString
  implicit val arbAlphaNumString: Arbitrary[String] = fj.test.Arbitrary.arbAlphaNumString
  implicit val arbStringBuffer: Arbitrary[StringBuffer] = fj.test.Arbitrary.arbStringBuffer
  implicit val arbStringBuilder: Arbitrary[java.lang.StringBuilder] = fj.test.Arbitrary.arbStringBuilder

  implicit def arbGen[A](implicit aa: Arbitrary[A]): Arbitrary[fj.test.Gen[A]] =
    fj.test.Arbitrary.arbGen(aa)

  implicit def arbOption[A](implicit aa: Arbitrary[A]): Arbitrary[fj.data.Option[A]] =
    fj.test.Arbitrary.arbOption(aa)

  implicit def arbEither[A, B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[fj.data.Either[A, B]] =
    fj.test.Arbitrary.arbEither(aa, ab)

  implicit def arbList[A](implicit aa: Arbitrary[A]): Arbitrary[fj.data.List[A]] =
    fj.test.Arbitrary.arbList(aa)

  implicit def arbStream[A](implicit aa: Arbitrary[A]): Arbitrary[fj.data.Stream[A]] =
    fj.test.Arbitrary.arbStream(aa)

  implicit def arbArray[A](implicit aa: Arbitrary[A]): Arbitrary[fj.data.Array[A]] =
    fj.test.Arbitrary.arbArray(aa)

  implicit def arbArrayList[A](implicit aa: Arbitrary[A]): Arbitrary[ArrayList[A]] =
    fj.test.Arbitrary.arbArrayList(aa)

  implicit val arbBitSet: Arbitrary[BitSet] = fj.test.Arbitrary.arbBitSet
  implicit val arbCalendar: Arbitrary[Calendar] = fj.test.Arbitrary.arbCalendar
  implicit val arbDate: Arbitrary[Date] = fj.test.Arbitrary.arbDate

  implicit def arbEnumMap[K <: Enum[K], V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[EnumMap[K, V]] =
    fj.test.Arbitrary.arbEnumMap(ak, av)

  implicit def arbEnumSet[A <: Enum[A]](implicit aa: Arbitrary[A]): Arbitrary[EnumSet[A]] =
    fj.test.Arbitrary.arbEnumSet(aa)
  
  implicit val arbGregorianCalendar: Arbitrary[GregorianCalendar] = fj.test.Arbitrary.arbGregorianCalendar

  implicit def arbHashMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[HashMap[K, V]] =
    fj.test.Arbitrary.arbHashMap(ak, av)

  implicit def arbHashSet[A](implicit aa: Arbitrary[A]): Arbitrary[HashSet[A]] =
    fj.test.Arbitrary.arbHashSet(aa)

  implicit def arbHashtable[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[Hashtable[K, V]] =
    fj.test.Arbitrary.arbHashtable(ak, av)

  implicit def arbIdentityHashMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[IdentityHashMap[K, V]] =
    fj.test.Arbitrary.arbIdentityHashMap(ak, av)

  implicit def arbLinkedHashMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[LinkedHashMap[K, V]] =
    fj.test.Arbitrary.arbLinkedHashMap(ak, av)

  implicit def arbLinkedHashSet[A](implicit aa: Arbitrary[A]): Arbitrary[LinkedHashSet[A]] =
    fj.test.Arbitrary.arbLinkedHashSet(aa)

  implicit def arbLinkedList[A](implicit aa: Arbitrary[A]): Arbitrary[LinkedList[A]] =
    fj.test.Arbitrary.arbLinkedList(aa)

  implicit def arbPriorityQueue[A](implicit aa: Arbitrary[A]): Arbitrary[PriorityQueue[A]] =
    fj.test.Arbitrary.arbPriorityQueue(aa)

  implicit val arbProperties: Arbitrary[Properties] = fj.test.Arbitrary.arbProperties

  implicit def arbStack[A](implicit aa: Arbitrary[A]): Arbitrary[Stack[A]] =
    fj.test.Arbitrary.arbStack(aa)

  implicit def arbTreeMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[TreeMap[K, V]] =
    fj.test.Arbitrary.arbTreeMap(ak, av)

  implicit def arbTreeSet[A](implicit aa: Arbitrary[A]): Arbitrary[TreeSet[A]] =
    fj.test.Arbitrary.arbTreeSet(aa)

  implicit def arbVector[A](implicit aa: Arbitrary[A]): Arbitrary[Vector[A]] =
    fj.test.Arbitrary.arbVector(aa)

  implicit def arbWeakHashMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[WeakHashMap[K, V]] =
    fj.test.Arbitrary.arbWeakHashMap(ak, av)

  implicit def arbArrayBlockingQueue[A](implicit aa: Arbitrary[A]): Arbitrary[ArrayBlockingQueue[A]] =
    fj.test.Arbitrary.arbArrayBlockingQueue(aa)

  implicit def arbConcurrentHashMap[K, V](implicit ak: Arbitrary[K], av: Arbitrary[V]): Arbitrary[ConcurrentHashMap[K, V]] =
    fj.test.Arbitrary.arbConcurrentHashMap(ak, av)

  implicit def arbConcurrentLinkedQueue[A](implicit aa: Arbitrary[A]): Arbitrary[ConcurrentLinkedQueue[A]] =
    fj.test.Arbitrary.arbConcurrentLinkedQueue(aa)

  implicit def arbCopyOnWriteArrayList[A](implicit aa: Arbitrary[A]): Arbitrary[CopyOnWriteArrayList[A]] =
    fj.test.Arbitrary.arbCopyOnWriteArrayList(aa)

  implicit def arbCopyOnWriteArraySet[A](implicit aa: Arbitrary[A]): Arbitrary[CopyOnWriteArraySet[A]] =
    fj.test.Arbitrary.arbCopyOnWriteArraySet(aa)

  implicit def arbDelayQueue[A <: java.util.concurrent.Delayed](implicit aa: Arbitrary[A]): Arbitrary[DelayQueue[A]] =
    fj.test.Arbitrary.arbDelayQueue(aa)

  implicit def arbLinkedBlockingQueue[A](implicit aa: Arbitrary[A]): Arbitrary[LinkedBlockingQueue[A]] =
    fj.test.Arbitrary.arbLinkedBlockingQueue(aa)

  implicit def arbPriorityBlockingQueue[A](implicit aa: Arbitrary[A]): Arbitrary[PriorityBlockingQueue[A]] =
    fj.test.Arbitrary.arbPriorityBlockingQueue(aa)

  implicit def arbSynchronousQueue[A](implicit aa: Arbitrary[A]): Arbitrary[SynchronousQueue[A]] =
    fj.test.Arbitrary.arbSynchronousQueue(aa)

  implicit val arbSQLDate: Arbitrary[java.sql.Date] = fj.test.Arbitrary.arbSQLDate
  implicit val arbTime: Arbitrary[Time] = fj.test.Arbitrary.arbTime
  implicit val arbTimestamp: Arbitrary[Timestamp] = fj.test.Arbitrary.arbTimestamp
  implicit val arbBigInteger: Arbitrary[BigInteger] = fj.test.Arbitrary.arbBigInteger
  implicit val arbBigDecimal: Arbitrary[BigDecimal] = fj.test.Arbitrary.arbBigDecimal

  implicit def arbLocale: Arbitrary[Locale] = fj.test.Arbitrary.arbLocale
  
  implicit def arbP1[A](implicit aa: Arbitrary[A]): Arbitrary[P1[A]] =
    fj.test.Arbitrary.arbP1(aa)

  implicit def arbP2[A, B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[P2[A, B]] =
    fj.test.Arbitrary.arbP2(aa, ab)

  implicit def arbP3[A, B, C](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C]): Arbitrary[P3[A, B, C]] =
    fj.test.Arbitrary.arbP3(aa, ab, ac)

  implicit def arbP4[A, B, C, D](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D]): Arbitrary[P4[A, B, C, D]] =
    fj.test.Arbitrary.arbP4(aa, ab, ac, ad)

  implicit def arbP5[A, B, C, D, E](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E]): Arbitrary[P5[A, B, C, D, E]] =
    fj.test.Arbitrary.arbP5(aa, ab, ac, ad, ae)

  implicit def arbP6[A, B, C, D, E, F$](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$]): Arbitrary[P6[A, B, C, D, E, F$]] =
    fj.test.Arbitrary.arbP6(aa, ab, ac, ad, ae, af)

  implicit def arbP7[A, B, C, D, E, F$, G](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G]): Arbitrary[P7[A, B, C, D, E, F$, G]] =
    fj.test.Arbitrary.arbP7(aa, ab, ac, ad, ae, af, ag)

  implicit def arbP8[A, B, C, D, E, F$, G, H](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], ah: Arbitrary[H]): Arbitrary[P8[A, B, C, D, E, F$, G, H]] =
    fj.test.Arbitrary.arbP8(aa, ab, ac, ad, ae, af, ag, ah)

  // BEGIN Scala

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

  implicit val arbSBoolean: Arbitrary[Boolean] = arbBoolean > ((_: java.lang.Boolean) booleanValue)
  implicit val arbSByte: Arbitrary[Byte] = arbByte > ((_: java.lang.Byte) byteValue)
  implicit val arbSChar: Arbitrary[Char] = arbCharacter > ((_: java.lang.Character) charValue)
  implicit val arbSDouble: Arbitrary[Double] = arbDouble > ((_: java.lang.Double) doubleValue)
  implicit val arbSFloat: Arbitrary[Float] = arbFloat > ((_: java.lang.Float) floatValue)
  implicit val arbSInt: Arbitrary[Int] = arbInteger > ((_: java.lang.Integer) intValue)
  implicit val arbSLong: Arbitrary[Long] = arbLong > ((_: java.lang.Long) longValue)
  implicit val arbSShort: Arbitrary[Short] = arbShort > ((_: java.lang.Short) shortValue)
  implicit val arbSStringBuilder: Arbitrary[StringBuilder] = arbStringBuilder > ((x: java.lang.StringBuilder) => new StringBuilder(x.toString))
  implicit def arbSOption[A](implicit a: Arbitrary[A]): Arbitrary[scala.Option[A]] = arbOption(a) > ((t: fj.data.Option[A]) => (t: scala.Option[A]))
  implicit def arbSEither[A, B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[scala.Either[A, B]] = arbEither(aa, ab) > ((t: fj.data.Either[A, B]) => (t: scala.Either[A, B]))
  implicit def arbSList[A](implicit a: Arbitrary[A]): Arbitrary[scala.List[A]] = arbList(a) > ((t: fj.data.List[A]) => (t: scala.List[A]))
  implicit def arbSStream[A](implicit a: Arbitrary[A]): Arbitrary[scala.Stream[A]] = arbStream(a) > ((t: fj.data.Stream[A]) => (t: scala.Stream[A]))
  implicit def arbSArray[A](implicit a: Arbitrary[A]): Arbitrary[scala.Array[A]] = arbArray(a) > ((t: fj.data.Array[A]) => (t: scala.Array[A]))

  object SBoundaries {
    implicit val arbSCharBoundaries: Arbitrary[Char] = fj.test.Arbitrary.arbCharacterBoundaries > ((_: java.lang.Character).charValue)
    implicit val arbSIntBoundaries: Arbitrary[Int] = fj.test.Arbitrary.arbIntegerBoundaries > ((_: java.lang.Integer).intValue)
    implicit val arbSLongBoundaries: Arbitrary[Long] = fj.test.Arbitrary.arbLongBoundaries > ((_: java.lang.Long).longValue)
    implicit val arbSByteBoundaries: Arbitrary[Byte] = fj.test.Arbitrary.arbByteBoundaries > ((_: java.lang.Byte).byteValue)
    implicit val arbSShortBoundaries: Arbitrary[Short] = fj.test.Arbitrary.arbShortBoundaries > ((_: java.lang.Short).shortValue)
    implicit val arbSDoubleBoundaries: Arbitrary[Double] = fj.test.Arbitrary.arbDoubleBoundaries > ((_: java.lang.Double).doubleValue)
    implicit val arbSFloatBoundaries: Arbitrary[Float] = fj.test.Arbitrary.arbFloatBoundaries > ((_: java.lang.Float).floatValue)
  }

  implicit def arbSTuple1[A](implicit a: Arbitrary[A]): Arbitrary[Tuple1[A]] =
    arbP1(a) > ((t: fj.P1[A]) => (t: Tuple1[A]))

  implicit def arbSTuple2[A, B](implicit aa: Arbitrary[A], ab: Arbitrary[B]): Arbitrary[Tuple2[A, B]] =
    arbP2(aa, ab) > ((t: fj.P2[A, B]) => (t: Tuple2[A, B]))

  implicit def arbSTuple3[A, B, C](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C]): Arbitrary[Tuple3[A, B, C]] =
    arbP3(aa, ab, ac) > ((t: fj.P3[A, B, C]) => (t: Tuple3[A, B, C]))

  implicit def arbSTuple4[A, B, C, D](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D]): Arbitrary[Tuple4[A, B, C, D]] =
    arbP4(aa, ab, ac, ad) > ((t: fj.P4[A, B, C, D]) => (t: Tuple4[A, B, C, D]))

  implicit def arbSTuple5[A, B, C, D, E](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E]): Arbitrary[Tuple5[A, B, C, D, E]] =
    arbP5(aa, ab, ac, ad, ae) > ((t: fj.P5[A, B, C, D, E]) => (t: Tuple5[A, B, C, D, E]))

  implicit def arbSTuple6[A, B, C, D, E, F$](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$]): Arbitrary[Tuple6[A, B, C, D, E, F$]] =
    arbP6(aa, ab, ac, ad, ae, af) > ((t: fj.P6[A, B, C, D, E, F$]) => (t: Tuple6[A, B, C, D, E, F$]))

  implicit def arbSTuple7[A, B, C, D, E, F$, G](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G]): Arbitrary[Tuple7[A, B, C, D, E, F$, G]] =
    arbP7(aa, ab, ac, ad, ae, af, ag) > ((t: fj.P7[A, B, C, D, E, F$, G]) => (t: Tuple7[A, B, C, D, E, F$, G]))

  implicit def arbSTuple8[A, B, C, D, E, F$, G, H](implicit aa: Arbitrary[A], ab: Arbitrary[B], ac: Arbitrary[C], ad: Arbitrary[D], ae: Arbitrary[E], af: Arbitrary[F$], ag: Arbitrary[G], ah: Arbitrary[H]): Arbitrary[Tuple8[A, B, C, D, E, F$, G, H]] =
    arbP8(aa, ab, ac, ad, ae, af, ag, ah) > ((t: fj.P8[A, B, C, D, E, F$, G, H]) => (t: Tuple8[A, B, C, D, E, F$, G, H]))

  implicit def arbSF[A, B](implicit c: Coarb[A], a: Arbitrary[B]): Arbitrary[A => B] = arbF(c, a) > ((x: fj.F[A, B]) => (x: A => B))
  
  implicit def arbSF2[A, B, C](implicit ca: Coarb[A], cb: Coarb[B], a: Arbitrary[C]): Arbitrary[(A, B) => C] = arbF2(ca, cb, a) > ((x: fj.F2[A, B, C]) => (x: (A, B) => C))

  implicit def arbSF3[A, B, C, D](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], a: Arbitrary[D]): Arbitrary[(A, B, C) => D] = arbF3(ca, cb, cc, a) > ((x: fj.F3[A, B, C, D]) => (x: (A, B, C) => D))

  implicit def arbSF4[A, B, C, D, E](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], a: Arbitrary[E]): Arbitrary[(A, B, C, D) => E] = arbF4(ca, cb, cc, cd, a) > ((x: fj.F4[A, B, C, D, E]) => (x: (A, B, C, D) => E))

  implicit def arbSF5[A, B, C, D, E, F$](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], a: Arbitrary[F$]): Arbitrary[(A, B, C, D, E) => F$] = arbF5(ca, cb, cc, cd, ce, a) > ((x: fj.F5[A, B, C, D, E, F$]) => (x: (A, B, C, D, E) => F$))

  implicit def arbSF6[A, B, C, D, E, F$, G](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], a: Arbitrary[G]): Arbitrary[(A, B, C, D, E, F$) => G] = arbF6(ca, cb, cc, cd, ce, cf, a) > ((x: fj.F6[A, B, C, D, E, F$, G]) => (x: (A, B, C, D, E, F$) => G))

  implicit def arbSF7[A, B, C, D, E, F$, G, H](implicit ca: Coarb[A], cb: Coarb[B], cc: Coarb[C], cd: Coarb[D], ce: Coarb[E], cf: Coarb[F$], cg: Coarb[G], a: Arbitrary[H]): Arbitrary[(A, B, C, D, E, F$, G) => H] = arbF7(ca, cb, cc, cd, ce, cf, cg, a) > ((x: fj.F7[A, B, C, D, E, F$, G, H]) => (x: (A, B, C, D, E, F$, G) => H))

  object SFInvariant {
    implicit def arbFInvariant[A, B](implicit a: Arbitrary[B]): Arbitrary[A => B] =
      fj.test.Arbitrary.arbFInvariant(a) > ((x: F[A, B]) => (x: A => B))

    implicit def arbF2Invariant[A, B, C](implicit a: Arbitrary[C]): Arbitrary[(A, B) => C] =
      fj.test.Arbitrary.arbF2Invariant(a) > ((x: F2[A, B, C]) => (x: (A, B) => C))

    implicit def arbF3Invariant[A, B, C, D](implicit a: Arbitrary[D]): Arbitrary[(A, B, C) => D] =
      fj.test.Arbitrary.arbF3Invariant(a) > ((x: F3[A, B, C, D]) => (x: (A, B, C) => D))

    implicit def arbF4Invariant[A, B, C, D, E](implicit a: Arbitrary[E]): Arbitrary[(A, B, C, D) => E] =
      fj.test.Arbitrary.arbF4Invariant(a) > ((x: F4[A, B, C, D, E]) => (x: (A, B, C, D) => E))

    implicit def arbF5Invariant[A, B, C, D, E, F$](implicit a: Arbitrary[F$]): Arbitrary[(A, B, C, D, E) => F$] =
      fj.test.Arbitrary.arbF5Invariant(a) > ((x: F5[A, B, C, D, E, F$]) => (x: (A, B, C, D, E) => F$))

    implicit def arbF6Invariant[A, B, C, D, E, F$, G](implicit a: Arbitrary[G]): Arbitrary[(A, B, C, D, E, F$) => G] =
      fj.test.Arbitrary.arbF6Invariant(a) > ((x: F6[A, B, C, D, E, F$, G]) => (x: (A, B, C, D, E, F$) => G))

    implicit def arbF7Invariant[A, B, C, D, E, F$, G, H](implicit a: Arbitrary[H]): Arbitrary[(A, B, C, D, E, F$, G) => H] =
      fj.test.Arbitrary.arbF7Invariant(a) > ((x: F7[A, B, C, D, E, F$, G, H]) => (x: (A, B, C, D, E, F$, G) => H))

    implicit def arbF8Invariant[A, B, C, D, E, F$, G, H, I](implicit a: Arbitrary[I]): Arbitrary[(A, B, C, D, E, F$, G, H) => I] =
      fj.test.Arbitrary.arbF8Invariant(a) > ((x: F8[A, B, C, D, E, F$, G, H, I]) => (x: (A, B, C, D, E, F$, G, H) => I))
  }

  // END Scala
}
