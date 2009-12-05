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

  implicit def IntMultiplicationOrder: Order[IntMultiplication] = IntOrder ∙ ((_: IntMultiplication).value)

  implicit def BooleanOrder: Order[Boolean] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def BooleanConjunctionOrder: Order[BooleanConjunction] = BooleanOrder ∙ ((_: BooleanConjunction).value)

  implicit def CharOrder: Order[Char] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def CharMultiplicationOrder: Order[CharMultiplication] = CharOrder ∙ ((_: CharMultiplication).value)

  implicit def ByteOrder: Order[Byte] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def ByteMultiplicationOrder: Order[ByteMultiplication] = ByteOrder ∙ ((_: ByteMultiplication).value)

  implicit def LongOrder: Order[Long] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def LongMultiplicationOrder: Order[LongMultiplication] = LongOrder ∙ ((_: LongMultiplication).value)

  implicit def ShortOrder: Order[Short] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def ShortMultiplicationOrder: Order[ShortMultiplication] = ShortOrder ∙ ((_: ShortMultiplication).value)

  implicit def FloatOrder: Order[Float] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def DoubleOrder: Order[Double] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def BigIntegerOrder: Order[BigInteger] = order(_ compareTo _ ordering)

  implicit def BigIntegerMultiplicationOrder: Order[BigIntegerMultiplication] = BigIntegerOrder ∙ ((_: BigIntegerMultiplication).value)

  implicit def BigIntOrder: Order[BigInt] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit def BigIntMultplicationOrder: Order[BigIntMultiplication] = BigIntOrder ∙ ((_: BigIntMultiplication).value)

  implicit def NonEmptyListOrder[A](implicit oa: Order[A]): Order[NonEmptyList[A]] = IterableOrder(oa) ∙ ((_: NonEmptyList[A]).list)

  implicit def ZipStreamOrder[A](implicit oa: Order[A]): Order[ZipStream[A]] = IterableOrder(oa) ∙ ((_: ZipStream[A]).value)

  implicit def Tuple1Order[A](implicit oa: Order[A]): Order[Tuple1[A]] = order(_._1 ?|? _._1)

  implicit def Tuple2Order[A, B](implicit oa: Order[A], ob: Order[B]): Order[(A, B)] = order {
    case ((a1, b1), (a2, b2)) => List(a1 ?|? a2, b1 ?|? b2) ∑
  }

  implicit def Tuple3Order[A, B, C](implicit oa: Order[A], ob: Order[B], oc: Order[C]): Order[(A, B, C)] = order {
    case ((a1, b1, c1), (a2, b2, c2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2) ∑
  }

  implicit def Tuple4Order[A, B, C, D](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D]): Order[(A, B, C, D)] = order {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2) ∑
  }

  implicit def Tuple5Order[A, B, C, D, E](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D], oe: Order[E]): Order[(A, B, C, D, E)] = order {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2) ∑
  }

  implicit def Tuple6Order[A, B, C, D, E, F](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D], oe: Order[E], of: Order[F]): Order[(A, B, C, D, E, F)] = order {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2, f1 ?|? f2) ∑
  }

  implicit def Tuple7Order[A, B, C, D, E, F, G](implicit oa: Order[A], ob: Order[B], oc: Order[C], od: Order[D], oe: Order[E], of: Order[F], og: Order[G]): Order[(A, B, C, D, E, F, G)] = order {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2, f1 ?|? f2, g1 ?|? g2) ∑
  }

  implicit def Function0Order[A](implicit oa: Order[A]): Order[Function0[A]] = order(_.apply ?|? _.apply)

  implicit def IterableOrder[A](implicit oa: Order[A]): Order[Iterable[A]] = order((a1, a2) => {
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

  implicit def OptionOrder[A](implicit o: Order[A]): Order[Option[A]] = order {
    case (Some(x), Some(y)) => x ?|? y
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  }

  implicit def EitherOrder[A, B](implicit oa: Order[A], ob: Order[B]): Order[Either[A, B]] = order {
    case (Left(x), Left(y)) => x ?|? y
    case (Right(x), Right(y)) => x ?|? y
    case (Left(_), Right(_)) => LT
    case (Right(_), Left(_)) => GT
  }

  implicit def EitherLeftOrder[A, B](implicit oa: Order[A], ob: Order[B]): Order[Either.LeftProjection[A, B]] = order((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ?|? a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def EitherRightOrder[A, B](implicit oa: Order[A], ob: Order[B]): Order[Either.RightProjection[A, B]] = order((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ?|? a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def ValidationOrder[E, A](implicit ee: Order[E], ea: Order[A]): Order[Validation[E, A]] = EitherOrder(ee, ea) ∙ ((_: Validation[E, A]).either)

  implicit def JavaIterableOrder[A](implicit oa: Order[A]): Order[java.lang.Iterable[A]] = IterableOrder(oa) ∙ (x => x)
}
