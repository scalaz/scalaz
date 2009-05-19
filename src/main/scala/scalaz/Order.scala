package scalaz

trait Order[-A] extends Equal[A] {
  def order(a1: A, a2: A): Ordering

  final def equal(a1: A, a2: A) = order(a1, a2) == EQ
}

object Order {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }

  import Scalaz._

  implicit val DigitOrder: Order[Digit] = IntOrder <| ((_: Digit).toInt)

  implicit val OrderingOrder: Order[Ordering] = order {
    case (a, EQ) => a
    case (EQ, LT) => GT
    case (EQ, GT) => LT
    case (LT, LT) => EQ
    case (LT, _) => LT
    case (GT, GT) => EQ
    case (GT, _) => GT
  }

  implicit val UnitOrder: Order[Unit] = order((_, _) => EQ)

  implicit val StringOrder: Order[String] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val IntOrder: Order[Int] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val IntMultiplicationOrder: Order[IntMultiplication] = IntOrder <| ((_: IntMultiplication).value)

  implicit val BooleanOrder: Order[Boolean] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val BooleanConjunctionOrder: Order[BooleanConjunction] = BooleanOrder <| ((_: BooleanConjunction).value)

  implicit val CharOrder: Order[Char] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val CharMultiplicationOrder: Order[CharMultiplication] = CharOrder <| ((_: CharMultiplication).value)

  implicit val ByteOrder: Order[Byte] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val ByteMultiplicationOrder: Order[ByteMultiplication] = ByteOrder <| ((_: ByteMultiplication).value)

  implicit val LongOrder: Order[Long] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val LongMultiplicationOrder: Order[LongMultiplication] = LongOrder <| ((_: LongMultiplication).value)

  implicit val ShortOrder: Order[Short] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val ShortMultiplicationOrder: Order[ShortMultiplication] = ShortOrder <| ((_: ShortMultiplication).value)

  implicit val FloatOrder: Order[Float] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val DoubleOrder: Order[Double] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val BigIntegerOrder: Order[java.math.BigInteger] = order(_ compareTo _ ordering)

  implicit val BigIntegerMultiplicationOrder: Order[BigIntegerMultiplication] = BigIntegerOrder <| ((_: BigIntegerMultiplication).value)

  implicit val BigIntOrder: Order[BigInt] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val BigIntMultplicationOrder: Order[BigIntMultiplication] = BigIntOrder <| ((_: BigIntMultiplication).value)

  implicit def NonEmptyListOrder[A](implicit oa: Order[A]): Order[NonEmptyList[A]] = IterableOrder(oa) <| ((_: NonEmptyList[A]).list)

  implicit def ZipStreamOrder[A](implicit oa: Order[A]): Order[ZipStream[A]] = IterableOrder(oa) <| ((_: ZipStream[A]).value)

  implicit def Tuple1Order[A](implicit oa: Order[A]) = order[Tuple1[A]](_._1 ?:? _._1)

  implicit def Tuple2Order[A, B](implicit oa: Order[A], ob: Order[B]) = order[(A, B)] {
    case ((a1, b1), (a2, b2)) => List(a1 ?:? a2, b1 ?:? b2) suml
  }

  implicit def Tuple3Order[A, B, C](implicit oa: Order[A], ob: Order[B], oc: Order[C]) = order[(A, B, C)] {
    case ((a1, b1, c1), (a2, b2, c2)) => List(a1 ?:? a2, b1 ?:? b2, c1 ?:? c2) suml
  }

  implicit def Tuple4Order[A, B, C, D](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D]) = order[(A, B, C, D)] {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => List(a1 ?:? a2, b1 ?:? b2, c1 ?:? c2, d1 ?:? d2) suml
  }

  implicit def Tuple5Order[A, B, C, D, E](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D], oe: Order[E]) = order[(A, B, C, D, E)] {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => List(a1 ?:? a2, b1 ?:? b2, c1 ?:? c2, d1 ?:? d2, e1 ?:? e2) suml
  }

  implicit def Tuple6Order[A, B, C, D, E, F](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D], oe: Order[E], of: Order[F]) = order[(A, B, C, D, E, F)] {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => List(a1 ?:? a2, b1 ?:? b2, c1 ?:? c2, d1 ?:? d2, e1 ?:? e2, f1 ?:? f2) suml
  }

  implicit def Tuple7Order[A, B, C, D, E, F, G](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D], oe: Order[E], of: Order[F], og: Order[G]) = order[(A, B, C, D, E, F, G)] {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => List(a1 ?:? a2, b1 ?:? b2, c1 ?:? c2, d1 ?:? d2, e1 ?:? e2, f1 ?:? f2, g1 ?:? g2) suml
  }

  implicit def Function0Order[A](implicit oa: Order[A]) = order[Function0[A]](_.apply ?:? _.apply)

  implicit def IterableOrder[A](implicit oa: Order[A]): Order[Iterable[A]] = order((a1, a2) => {
    val i1 = a1.elements
    val i2 = a2.elements
    var b = true
    var r: Ordering = EQ

    while(i1.hasNext && i2.hasNext && b) {
      val a1 = i1.next
      val a2 = i2.next

      val o = a1 ?:? a2
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

  implicit def OptionOrder[A](implicit o: Order[A]): Order[Option[A]] = order[Option[A]] {
    case (Some(x), Some(y)) => x ?:? y
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  }

  implicit def EitherOrder[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either[A, B]] {
    case (Left(x), Left(y)) => x ?:? y
    case (Right(x), Right(y)) => x ?:? y
    case (Left(_), Right(_)) => LT
    case (Right(_), Left(_)) => GT
  }

  implicit def EitherLeftOrder[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either.LeftProjection[A, B]]((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ?:? a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def EitherRightOrder[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either.RightProjection[A, B]]((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ?:? a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def ValidationOrder[E, A](implicit ee: Order[E], ea: Order[A]) = EitherOrder(ee, ea) <| ((_: Validation[E, A]).either)  

  implicit def JavaIterableOrder[A](implicit oa: Order[A]): Order[java.lang.Iterable[A]] = IterableOrder(oa) <| (x => JavaIterableTo(x))
}
