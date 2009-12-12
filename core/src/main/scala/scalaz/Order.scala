package scalaz

trait Order[-A] extends Equal[A] {
  def order(a1: A, a2: A): Ordering

  final def equal(a1: A, a2: A) = order(a1, a2) == EQ
}

trait Orders {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }              
}

object Order {
  import Scalaz._
  import java.math.BigInteger

  implicit def DigitOrder: Order[Digit] = IntOrder ∙ ((_: Digit).toInt)

  implicit def OrderingOrder: Order[Ordering] = order {
    case (a, EQ) => a
    case (EQ, LT) => GT
    case (EQ, GT) => LT
    case (LT, LT) => EQ
    case (LT, _) => LT
    case (GT, GT) => EQ
    case (GT, _) => GT
  }

  implicit def UnitOrder: Order[Unit] = order((_, _) => EQ)

  implicit def StringOrder: Order[String] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def IntOrder: Order[Int] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def IntMultiplicationOrder: Order[IntMultiplication] = i[Order, Int] ∙ ((_: IntMultiplication).value)

  implicit def BooleanOrder: Order[Boolean] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def BooleanConjunctionOrder: Order[BooleanConjunction] = i[Order, Boolean] ∙ ((_: BooleanConjunction).value)

  implicit def CharOrder: Order[Char] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def CharMultiplicationOrder: Order[CharMultiplication] = i[Order, Char] ∙ ((_: CharMultiplication).value)

  implicit def ByteOrder: Order[Byte] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def ByteMultiplicationOrder: Order[ByteMultiplication] = i[Order, Byte] ∙ ((_: ByteMultiplication).value)

  implicit def LongOrder: Order[Long] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def LongMultiplicationOrder: Order[LongMultiplication] = i[Order, Long] ∙ ((_: LongMultiplication).value)

  implicit def ShortOrder: Order[Short] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def ShortMultiplicationOrder: Order[ShortMultiplication] = i[Order, Short] ∙ ((_: ShortMultiplication).value)

  implicit def FloatOrder: Order[Float] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def DoubleOrder: Order[Double] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def BigIntegerOrder: Order[BigInteger] = order(_ compareTo _ ordering)

  implicit def BigIntegerMultiplicationOrder: Order[BigIntegerMultiplication] = i[Order, BigInteger] ∙ ((_: BigIntegerMultiplication).value)

  implicit def BigIntOrder: Order[BigInt] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def BigIntMultplicationOrder: Order[BigIntMultiplication] = i[Order, BigInt] ∙ ((_: BigIntMultiplication).value)

  implicit def NonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] = IterableOrder[A] ∙ ((_: NonEmptyList[A]).list)

  implicit def ZipStreamOrder[A: Order]: Order[ZipStream[A]] = IterableOrder[A] ∙ ((_: ZipStream[A]).value)

  implicit def Tuple1Order[A: Order]: Order[Tuple1[A]] = order(_._1 ?|? _._1)

  implicit def Tuple2Order[A: Order, B: Order]: Order[(A, B)] = order {
    case ((a1, b1), (a2, b2)) => List(a1 ?|? a2, b1 ?|? b2) ∑
  }

  implicit def Tuple3Order[A: Order, B: Order, C: Order]: Order[(A, B, C)] = order {
    case ((a1, b1, c1), (a2, b2, c2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2) ∑
  }

  implicit def Tuple4Order[A: Order, B: Order, C: Order, D: Order]: Order[(A, B, C, D)] = order {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2) ∑
  }

  implicit def Tuple5Order[A: Order, B: Order, C: Order, D: Order, E: Order]: Order[(A, B, C, D, E)] = order {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2) ∑
  }

  implicit def Tuple6Order[A: Order, B: Order, C: Order, D: Order, E: Order, F: Order]: Order[(A, B, C, D, E, F)] = order {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2, f1 ?|? f2) ∑
  }

  implicit def Tuple7Order[A: Order, B: Order, C: Order, D: Order, E: Order, F: Order, G: Order]: Order[(A, B, C, D, E, F, G)] = order {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2, f1 ?|? f2, g1 ?|? g2) ∑
  }

  implicit def Function0Order[A: Order]: Order[Function0[A]] = order(_.apply ?|? _.apply)

  implicit def IterableOrder[A: Order]: Order[Iterable[A]] = order((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = true
    var r: Ordering = EQ

    while(i1.hasNext && i2.hasNext && b) {
      val a1 = i1.next
      val a2 = i2.next

      val o = a1 ?|? a2
      if(o != EQ) {
        r = o
        b = false
      }
    }

    if(i1.hasNext)
      if(i2.hasNext)
        r
      else
        GT
    else
      LT
  })

  implicit def OptionOrder[A: Order]: Order[Option[A]] = order {
    case (Some(x), Some(y)) => x ?|? y
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  }

  implicit def EitherOrder[A: Order, B: Order]: Order[Either[A, B]] = order {
    case (Left(x), Left(y)) => x ?|? y
    case (Right(x), Right(y)) => x ?|? y
    case (Left(_), Right(_)) => LT
    case (Right(_), Left(_)) => GT
  }

  implicit def EitherLeftOrder[A: Order, B]: Order[Either.LeftProjection[A, B]] = order((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ?|? a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def EitherRightOrder[A, B: Order]: Order[Either.RightProjection[A, B]] = order((b1, b2) => (b1.toOption, b2.toOption) match {
    case (Some(b1), Some(b2)) => b1 ?|? b2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def ValidationOrder[E: Order, A: Order]: Order[Validation[E, A]] = EitherOrder[E, A] ∙ ((_: Validation[E, A]).either)

  implicit def JavaIterableOrder[A: Order]: Order[java.lang.Iterable[A]] = IterableOrder[A] ∙ (x => x)

  import geo._

  implicit def AzimuthOrder: Order[Azimuth] = i[Order, Double] ∙ (_.value)

  implicit def BearingOrder: Order[Bearing] = i[Order, Double] ∙ (_.value)

  implicit def CoordOrder: Order[Coord] = i[Order, (Latitude, Longitude)] ∙ (((_: Coord).latitude) &&& ((_: Coord).longitude))

  implicit def ElevatedCurveOrder: Order[ElevatedCurve] = i[Order, (GeodeticCurve, Elevation)] ∙ (((_: ElevatedCurve).curve) &&& ((_: ElevatedCurve).elevation))

  implicit def ElevationOrder: Order[Elevation] = i[Order, Double] ∙ (_.value)

  implicit def EllipsoidOrder: Order[Ellipsoid] = i[Order, (Double, Double, Double, Double)] ∙ (e => (e.semiMajor, e.semiMinor, e.flattening, e.inverseFlattening))

  implicit def GeodeticCurveOrder: Order[GeodeticCurve] = i[Order, (Double, Azimuth, Azimuth)] ∙ (c => (c.ellipsoidalDistance, c.azi, c.reverseAzi))

  implicit def LatitudeOrder: Order[Latitude] = i[Order, Double] ∙ (_.value)

  implicit def LongitudeOrder: Order[Longitude] = i[Order, Double] ∙ (_.value)

  implicit def PositionOrder: Order[Position] = i[Order, (Coord, Elevation)] ∙ (((_: Position).coord) &&& ((_: Position).elevation))

  implicit def VectorOrder: Order[Vector] = i[Order, (Coord, Bearing)] ∙ (((_: Vector).coord) &&& ((_: Vector).bearing))
}
