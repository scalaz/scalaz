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

  implicit def NonEmptyListEqual[A](implicit ea: Equal[A]): Equal[NonEmptyList[A]] = IterableEqual(ea) <| ((_: NonEmptyList[A]).list)

  implicit def OptionEqual[A](implicit ea: Equal[A]) = equal[Option[A]] {
    case (Some(a1), Some(a2)) => a1 === a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  }

  implicit def EitherEqual[A, B](implicit ea: Equal[A], eb: Equal[B]) = equal[Either[A, B]] {
    case(Left(a1), Left(a2)) => a1 === a2
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

  implicit def IterableEqual[A](implicit ea: Equal[A]) = equal[Iterable[A]]((a1, a2) => {
    val i1 = a1.elements
    val i2 = a2.elements
    var b = false

    while(i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if(x1 /= x2) {
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

    while(i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next
      val x2 = i2.next

      if(x1 /= x2) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  })

  implicit def JavaMapEntry[K, V](implicit ek: Equal[K], ev: Equal[V]) = equal[java.util.Map.Entry[K, V]]((a1, a2) => a1.getKey === a2.getKey)

  implicit def JavaMapEqual[K, V](implicit ek: Equal[K], ev: Equal[V]) = equal[java.util.Map[K, V]](_.entrySet === _.entrySet)
}
