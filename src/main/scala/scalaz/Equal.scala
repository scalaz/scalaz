package scalaz

trait Equal[-A] {
  def equal(a1: A, a2: A): Boolean
}

object Equal {
  def equal[A](f: (A, A) => Boolean): Equal[A] = new Equal[A] {
    def equal(a1: A, a2: A) = f(a1, a2)
  }

  def equalA[A] = equal[A](_ == _)

  import S._
  import MA._

  implicit val DigitEqual = equalA[Digit]

  implicit val OrderingEqual = equalA[Ordering]

  implicit val UnitEqual = equalA[Unit]

  implicit val StringEqual = equalA[String]

  implicit val IntEqual = equalA[Int]

  implicit val IntMultiplicationEqual = IntEqual <| ((_: IntMultiplication).value)

  implicit val BooleanEqual = equalA[Boolean]

  implicit val BooleanConjunctionEqual = BooleanEqual <| ((_: BooleanConjunction).value)

  implicit val CharEqual = equalA[Char]

  implicit val CharMultiplicationEqual = CharEqual <| ((_: CharMultiplication).value)

  implicit val ByteEqual = equalA[Byte]

  implicit val ByteMultiplicationEqual = ByteEqual <| ((_: ByteMultiplication).value)

  implicit val LongEqual = equalA[Long]

  implicit val LongMultiplicationEqual = LongEqual <| ((_: LongMultiplication).value)

  implicit val ShortEqual = equalA[Short]

  implicit val ShortMultiplicationEqual = ShortEqual <| ((_: ShortMultiplication).value)

  implicit val FloatEqual = equalA[Float]

  implicit val DoubleEqual = equalA[Double]

  implicit val BigIntegerEqual = equalA[java.math.BigInteger]

  implicit val BigIntegerMultiplicationEqual = BigIntegerEqual <| ((_: BigIntegerMultiplication).value)

  implicit val BigIntEqual = equalA[BigInt]

  implicit val BigIntMultiplicationEqual = BigIntEqual <| ((_: BigIntMultiplication).value)

  implicit val NodeSeqEqual = equalA[xml.NodeSeq]

  implicit def NonEmptyListEqual[A](implicit ea: Equal[A]): Equal[NonEmptyList[A]] = IterableEqual(ea) <| ((_: NonEmptyList[A]).list)

  implicit def ZipStreamEqual[A](implicit ea: Equal[A]): Equal[ZipStream[A]] = IterableEqual(ea) <| ((_: ZipStream[A]).value)

  implicit def Tuple1Equal[A](implicit ea: Equal[A]) = equal[Tuple1[A]](_._1 === _._1)

  implicit def Tuple2Equal[A, B](implicit ea: Equal[A], eb: Equal[B]) = equal[(A, B)] {
    case ((a1, b1), (a2, b2)) => a1 === a2 && b1 === b2
  }

  implicit def Tuple3Equal[A, B, C](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C]) = equal[(A, B, C)] {
    case ((a1, b1, c1), (a2, b2, c2)) => a1 === a2 && b1 === b2 && c1 === c2
  }

  implicit def Tuple4Equal[A, B, C, D](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D]) = equal[(A, B, C, D)] {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2
  }

  implicit def Tuple5Equal[A, B, C, D, E](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E]) = equal[(A, B, C, D, E)] {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2
  }

  implicit def Tuple6Equal[A, B, C, D, E, F](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F]) = equal[(A, B, C, D, E, F)] {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2
  }

  implicit def Tuple7Equal[A, B, C, D, E, F, G](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F], eg: Equal[G]) = equal[(A, B, C, D, E, F, G)] {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => a1 === a2 && b1 === b2 && c1 === c2 && d1 === d2 && e1 === e2 && f1 === f2 && g1 === g2
  }

  implicit def Function0Equal[A](implicit ea: Equal[A]) = equal[Function0[A]](_.apply === _.apply)

  implicit def OptionEqual[A](implicit ea: Equal[A]) = equal[Option[A]] {
    case (Some(a1), Some(a2)) => a1 === a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  }

  implicit def EitherEqual[A, B](implicit ea: Equal[A], eb: Equal[B]) = equal[Either[A, B]] {
    case (Left(a1), Left(a2)) => a1 === a2
    case (Right(b1), Right(b2)) => b1 === b2
    case _ => false
  }

  implicit def EitherLeftEqual[A, X](implicit ea: Equal[A]) = equal[Either.LeftProjection[A, X]]((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 === a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  })

  implicit def EitherRightEqual[X, A](implicit ea: Equal[A]) = equal[Either.RightProjection[X, A]]((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 === a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  })

  implicit def TreeEqual[A](implicit ea: Equal[A]): Equal[Tree[A]] =
    equal[Tree[A]]((a1, a2) => a1.rootLabel === a2.rootLabel
        && IterableEqual[Tree[A]].equal(a1.subForest, a2.subForest))

  implicit def IterableEqual[A](implicit ea: Equal[A]) = equal[Iterable[A]]((a1, a2) => {
    val i1 = a1.elements
    val i2 = a2.elements
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 /= x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def JavaIterableEqual[A](implicit ea: Equal[A]) = equal[java.lang.Iterable[A]]((a1, a2) => {
    import S._

    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if (x1 /= x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def JavaMapEntry[K, V](implicit ek: Equal[K], ev: Equal[V]) = equal[java.util.Map.Entry[K, V]]((a1, a2) => a1.getKey === a2.getKey)

  implicit def JavaMapEqual[K, V](implicit ek: Equal[K], ev: Equal[V]) = equal[java.util.Map[K, V]](_.entrySet === _.entrySet)
}
