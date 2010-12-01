package scalaz

trait Order[-A] extends Equal[A] {
  def order(a1: A, a2: A): Ordering

  final def equal(a1: A, a2: A): Boolean = order(a1, a2) == EQ
}

trait Orders {
  import Scalaz._

  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }

  def orderBy[A, B: Order](f: A => B): Order[A] = implicitly[Order[B]] ∙ f

}

trait OrderLow {
  import Scalaz._

  implicit def ScalaOrderingOrder[T: scala.Ordering]: Order[T] = order {(t1, t2) =>
    implicitly[scala.Ordering[T]].compare(t1, t2) match {
      case -1 => LT
      case 0 => EQ
      case 1 => GT
    }
  }
}

object Order {
  import Scalaz._
  import java.math.BigInteger

  implicit def DigitOrder: Order[Digit] = orderBy(_.toInt)

  implicit def OrderingOrder: Order[Ordering] = order {
    case (a, EQ) => a
    case (EQ, LT) => GT
    case (EQ, GT) => LT
    case (LT, LT) => EQ
    case (LT, _) => LT
    case (GT, GT) => EQ
    case (GT, _) => GT
  }

  implicit def OrderOrdering[A: Order]: scala.Ordering[A] = new scala.Ordering[A] {
    def compare(a1: A, a2: A) = (a1 ?|? a2) match {
      case EQ => 0
      case LT => -1
      case GT => 1
    }
  }

  implicit def UnitOrder: Order[Unit] = order((_, _) => EQ)

  implicit def StringOrder: Order[String] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def SymbolOrder: Order[Symbol] = orderBy(_.name)

  implicit def IntOrder: Order[Int] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def IntMultiplicationOrder: Order[IntMultiplication] = orderBy(_.value)

  implicit def BooleanOrder: Order[Boolean] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def BooleanConjunctionOrder: Order[BooleanConjunction] = orderBy(_.value)

  implicit def CharOrder: Order[Char] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def CharMultiplicationOrder: Order[CharMultiplication] = orderBy(_.value)

  implicit def ByteOrder: Order[Byte] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def ByteMultiplicationOrder: Order[ByteMultiplication] = orderBy(_.value)

  implicit def LongOrder: Order[Long] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def LongMultiplicationOrder: Order[LongMultiplication] = orderBy(_.value)

  implicit def ShortOrder: Order[Short] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def ShortMultiplicationOrder: Order[ShortMultiplication] = orderBy(_.value)

  implicit def FloatOrder: Order[Float] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def DoubleOrder: Order[Double] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def BigIntegerOrder: Order[BigInteger] = order(_ compareTo _ ordering)

  implicit def BigIntegerMultiplicationOrder: Order[BigIntegerMultiplication] = orderBy(_.value)

  implicit def BigIntOrder: Order[BigInt] = order((a1, a2) => if (a1 > a2) GT else if (a1 < a2) LT else EQ)

  implicit def BigIntMultplicationOrder: Order[BigIntMultiplication] = orderBy(_.value)

  implicit def NonEmptyListOrder[A: Order]: Order[NonEmptyList[A]] = orderBy(_.list)

  implicit def ZipStreamOrder[A: Order]: Order[ZipStream[A]] = orderBy(_.value)

  implicit def Tuple1Order[A: Order]: Order[Tuple1[A]] = order(_._1 ?|? _._1)

  import Foldable._

  implicit def Tuple2Order[A: Order, B: Order]: Order[(A, B)] = order {
    case ((a1, b1), (a2, b2)) => ListFoldable.fold(List(a1 ?|? a2, b1 ?|? b2))
  }

  implicit def Tuple3Order[A: Order, B: Order, C: Order]: Order[(A, B, C)] = order {
    case ((a1, b1, c1), (a2, b2, c2)) => ListFoldable.fold(List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2))
  }

  implicit def Tuple4Order[A: Order, B: Order, C: Order, D: Order]: Order[(A, B, C, D)] = order {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => ListFoldable.fold(List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2))
  }

  implicit def Tuple5Order[A: Order, B: Order, C: Order, D: Order, E: Order]: Order[(A, B, C, D, E)] = order {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => ListFoldable.fold(List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2))
  }

  implicit def Tuple6Order[A: Order, B: Order, C: Order, D: Order, E: Order, F: Order]: Order[(A, B, C, D, E, F)] = order {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => ListFoldable.fold(List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2, f1 ?|? f2))
  }

  implicit def Tuple7Order[A: Order, B: Order, C: Order, D: Order, E: Order, F: Order, G: Order]: Order[(A, B, C, D, E, F, G)] = order {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => ListFoldable.fold(List(a1 ?|? a2, b1 ?|? b2, c1 ?|? c2, d1 ?|? d2, e1 ?|? e2, f1 ?|? f2, g1 ?|? g2))
  }

  implicit def Function0Order[A: Order]: Order[() => A] = order(_.apply ?|? _.apply)

  implicit def IterableOrder[A: Order]: Order[Iterable[A]] = order((a1, a2) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = true
    var r: Ordering = EQ

    while (i1.hasNext && i2.hasNext && b) {
      val a1 = i1.next
      val a2 = i2.next

      val o = a1 ?|? a2
      if (o != EQ) {
        r = o
        b = false
      }
    }

    if (i1.hasNext)
      if (i2.hasNext)
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

  implicit def EitherLeftOrder[A: Order, B]: Order[Either.LeftProjection[A, B]] = order((a1, a2) =>
    (a1.toOption, a2.toOption) match {
      case (Some(a1), Some(a2)) => a1 ?|? a2
      case (Some(_), None) => GT
      case (None, Some(_)) => LT
      case (None, None) => EQ
    })

  implicit def EitherRightOrder[A, B: Order]: Order[Either.RightProjection[A, B]] = order((b1, b2) =>
    (b1.toOption, b2.toOption) match {
      case (Some(b1), Some(b2)) => b1 ?|? b2
      case (Some(_), None) => GT
      case (None, Some(_)) => LT
      case (None, None) => EQ
    })

  implicit def ValidationOrder[E: Order, A: Order]: Order[Validation[E, A]] = orderBy(_.either)

  implicit def JavaIterableOrder[A: Order]: Order[java.lang.Iterable[A]] = {
    import collection.JavaConversions._
    IterableOrder[A] ∙ (x => x)
  }
}
