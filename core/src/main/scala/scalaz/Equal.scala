package scalaz

/**
 * A type safe alternative to {@scala.Any.==}.
 * <p/>
 * For example:
 * <pre>
 *   (1 ≟ 0) ≟ false 
 *   (List("1") ≟ List("1")) ≟ true
 *   (1 ≟ "1") // compile error
 *   (1 ≟ 0L) // compile error
 * </pre>
 * @see scalaz.Identity#≟
 * @see scalaz.Identity#≠
 */
trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

trait Equals {
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  def equalA[A] = equal[A](_ == _)
}

object Equal {
  import Scalaz._
  import java.math.BigInteger
  import xml.NodeSeq
  import Predef.{implicitly => i}

  implicit def DigitEqual: Equal[Digit] = equalA

  implicit def OrderingEqual: Equal[Ordering] = equalA

  implicit def StringEqual: Equal[String] = equalA

  def NewTypeEqual[B: Equal, A <: NewType[B]]: Equal[A] = implicitly[Equal[B]] ∙ ((_: NewType[B]).value)

  implicit def UnitEqual: Equal[Unit] = equalA

  implicit def IntEqual: Equal[Int] = equalA

  implicit def ByteEqual: Equal[Byte] = equalA

  implicit def BooleanEqual: Equal[Boolean] = equalA

  implicit def CharEqual: Equal[Char] = equalA

  implicit def LongEqual: Equal[Long] = equalA

  implicit def ShortEqual: Equal[Short] = equalA

  implicit def FloatEqual: Equal[Float] = equalA

  implicit def DoubleEqual: Equal[Double] = equalA

  implicit def IntMultiplicationEqual: Equal[IntMultiplication] = NewTypeEqual[Int, IntMultiplication]

  implicit def BooleanConjunctionEqual: Equal[BooleanConjunction] = NewTypeEqual[Boolean, BooleanConjunction]

  implicit def CharMultiplicationEqual: Equal[CharMultiplication] = NewTypeEqual[Char, CharMultiplication]

  implicit def ByteMultiplicationEqual: Equal[ByteMultiplication] = NewTypeEqual[Byte, ByteMultiplication]

  implicit def LongMultiplicationEqual: Equal[LongMultiplication] = NewTypeEqual[Long, LongMultiplication]

  implicit def ShortMultiplicationEqual: Equal[ShortMultiplication] = NewTypeEqual[Short, ShortMultiplication]

  implicit def BigIntegerEqual: Equal[BigInteger] = equalA[java.math.BigInteger]

  implicit def BigIntegerMultiplicationEqual: Equal[BigIntegerMultiplication] = NewTypeEqual[BigInteger, BigIntegerMultiplication]

  implicit def BigIntEqual: Equal[BigInt] = equalA

  implicit def BigIntMultiplicationEqual: Equal[BigIntMultiplication] = NewTypeEqual[BigInt, BigIntMultiplication]

  implicit def NodeSeqEqual: Equal[NodeSeq] = equalA

  implicit def NonEmptyListEqual[A: Equal]: Equal[NonEmptyList[A]] = IterableEqual[A] ∙ ((_: NonEmptyList[A]).list)

  implicit def ZipStreamEqual[A: Equal]: Equal[ZipStream[A]] = IterableEqual[A] ∙ ((_: ZipStream[A]).value)

  implicit def Tuple1Equal[A: Equal]: Equal[Tuple1[A]] = equal(_._1 ≟ _._1)

  implicit def Tuple2Equal[A: Equal, B: Equal]: Equal[(A, B)] = equal {
    case ((a1, b1), (a2, b2)) => a1 ≟ a2 && b1 ≟ b2
  }

  implicit def Tuple3Equal[A: Equal, B: Equal, C: Equal]: Equal[(A, B, C)] = equal {
    case ((a1, b1, c1), (a2, b2, c2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2
  }

  implicit def Tuple4Equal[A: Equal, B: Equal, C: Equal, D: Equal]: Equal[(A, B, C, D)] = equal {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2
  }

  implicit def Tuple5Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal]: Equal[(A, B, C, D, E)] = equal {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2
  }

  implicit def Tuple6Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal]: Equal[(A, B, C, D, E, F)] = equal {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2 && f1 ≟ f2
  }

  implicit def Tuple7Equal[A: Equal, B: Equal, C: Equal, D: Equal, E: Equal, F: Equal, G: Equal]: Equal[(A, B, C, D, E, F, G)] = equal {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2 && f1 ≟ f2 && g1 ≟ g2
  }                             

  implicit def Function0Equal[A: Equal]: Equal[Function0[A]] = equal(_.apply ≟ _.apply)

  implicit def OptionEqual[A: Equal]: Equal[Option[A]] = equal {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  }

  implicit def OptionFirstEqual[A: Equal]: Equal[FirstOption[A]] = OptionEqual[A] ∙ ((_: FirstOption[A]).value)

  implicit def OptionLastEqual[A: Equal]: Equal[LastOption[A]] = OptionEqual[A] ∙ ((_: LastOption[A]).value)

  implicit def EitherEqual[A: Equal, B: Equal]: Equal[Either[A, B]] = equal {
    case (Left(a1), Left(a2)) => a1 ≟ a2
    case (Right(b1), Right(b2)) => b1 ≟ b2
    case _ => false
  }

  implicit def EitherLeftEqual[A: Equal, X]: Equal[Either.LeftProjection[A, X]] = equal((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  })

  implicit def EitherRightEqual[X, A: Equal]: Equal[Either.RightProjection[X, A]] = equal((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  })

  implicit def ValidationEqual[E: Equal, A: Equal]: Equal[Validation[E, A]] = EitherEqual[E, A] ∙ ((_: Validation[E, A]).either)

  implicit def FailProjectionEqual[E: Equal, A: Equal]: Equal[FailProjection[E, A]] = ValidationEqual[E, A] ∙ ((_: FailProjection[E, A]).validation)

  implicit def TreeEqual[A: Equal]: Equal[Tree[A]] =
    equal[Tree[A]]((a1, a2) => a1.rootLabel ≟ a2.rootLabel
        && IterableEqual[Tree[A]].equal(a1.subForest, a2.subForest))

  implicit def TreeLocEqual[A: Equal]: Equal[TreeLoc[A]] = {
    equal[TreeLoc[A]]((a1, a2) => a1.tree ≟ a2.tree
        && a1.lefts ≟ a2.lefts && a1.rights ≟ a2.rights && a1.parents ≟ a2.parents)
  }

  import concurrent.Promise
  implicit def PromiseEqual[A: Equal]: Equal[Promise[A]] =
    equal[Promise[A]]((a1, a2) => a1.get ≟ a2.get)

  def IterableEqual[A: Equal]: Equal[Iterable[A]] = equal((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 ≠ x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def ListEqual[A: Equal]: Equal[List[A]] = IterableEqual[A]

  implicit def StreamEqual[A: Equal]: Equal[Stream[A]] = IterableEqual[A]

  implicit def GArrayEqual[A: Equal]: Equal[GArray[A]] = IterableEqual[A]

  import geo._

  implicit def AzimuthEqual: Equal[Azimuth] = i[Equal[Double]] ∙ (_.value)

  implicit def BearingEqual: Equal[Bearing] = i[Equal[Double]] ∙ (_.value)

  implicit def CoordEqual: Equal[Coord] = i[Equal[(Latitude, Longitude)]] ∙ (((_: Coord).latitude) &&& ((_: Coord).longitude))

  implicit def ElevatedCurveEqual: Equal[ElevatedCurve] = i[Equal[(GeodeticCurve, Elevation)]] ∙ (((_: ElevatedCurve).curve) &&& ((_: ElevatedCurve).elevation))

  implicit def ElevationEqual: Equal[Elevation] = i[Equal[Double]] ∙ (_.value)

  implicit def EllipsoidEqual: Equal[Ellipsoid] = i[Equal[(Double, Double, Double, Double)]] ∙ (e => (e.semiMajor, e.semiMinor, e.
          flattening, e.inverseFlattening))

  implicit def GeodeticCurveEqual: Equal[GeodeticCurve] = i[Equal[(Double, Azimuth, Azimuth)]] ∙ (c => (c.ellipsoidalDistance, c.azi, c.reverseAzi))

  implicit def LatitudeEqual: Equal[Latitude] = i[Equal[Double]] ∙ (_.value)

  implicit def LongitudeEqual: Equal[Longitude] = i[Equal[Double]] ∙ (_.value)

  implicit def PositionEqual: Equal[Position] = i[Equal[(Coord, Elevation)]] ∙ (((_: Position).coord) &&& ((_: Position).elevation))

  implicit def VectorEqual: Equal[Vector] = i[Equal[(Coord, Bearing)]] ∙ (((_: Vector).coord) &&& ((_: Vector).bearing))

  implicit def JavaIterableEqual[A: Equal]: Equal[java.lang.Iterable[A]] = equal((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 ≠ x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def JavaMapEntry[K: Equal, V: Equal]: Equal[java.util.Map.Entry[K, V]] = equal((a1, a2) => a1.getKey ≟ a2.getKey)

  implicit def JavaMapEqual[K: Equal, V: Equal]: Equal[java.util.Map[K, V]] = equal(_.entrySet ≟ _.entrySet)

  implicit def CallableEqual[A: Equal]: Equal[java.util.concurrent.Callable[A]] = equal(_.call ≟ _.call)

  implicit def ZipperEqual[A: Equal]: Equal[Zipper[A]] = implicitly[Equal[Stream[A]]] ∙ ((z: Zipper[A]) => z.stream)
}
