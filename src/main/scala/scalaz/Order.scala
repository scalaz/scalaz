package scalaz

trait Order[-A] extends Equal[A] {
  def order(a1: A, a2: A): Ordering

  final def equal(a1: A, a2: A) = order(a1, a2) == EQ
}

object Order {
  def order[A](f: (A, A) => Ordering): Order[A] = new Order[A] {
    def order(a1: A, a2: A) = f(a1, a2)
  }

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

  implicit val BooleanOrder: Order[Boolean] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val CharOrder: Order[Char] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val ByteOrder: Order[Byte] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val LongOrder: Order[Long] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val ShortOrder: Order[Short] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val FloatOrder: Order[Float] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  implicit val DoubleOrder: Order[Double] = order((a1, a2) => if(a1 > a2) GT else if(a1 < a2) LT else EQ)

  import Identity._
  import MA._

  implicit def NonEmptyListOrder[A](implicit oa: Order[A]): Order[NonEmptyList[A]] = IterableOrder(oa) <| ((_: NonEmptyList[A]).list)

  implicit def IterableOrder[A](implicit oa: Order[A]): Order[Iterable[A]] = order((a1, a2) => {
    val i1 = a1.elements
    val i2 = a2.elements
    var b = true
    var r: Ordering = EQ

    while(i1.hasNext && i2.hasNext && b) {
      val a1 = i1.next
      val a2 = i2.next

      val o = a1 compare a2
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
    case (Some(x), Some(y)) => x compare y
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  }

  implicit def EitherOrder[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either[A, B]] {
    case (Left(x), Left(y)) => x compare y
    case (Right(x), Right(y)) => x compare y
    case (Left(_), Right(_)) => LT
    case (Right(_), Left(_)) => GT
  }

  implicit def EitherLeftOrder[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either.LeftProjection[A, B]]((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 compare a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  implicit def EitherRightOrder[A, B](implicit oa: Order[A], ob: Order[B]) = order[Either.RightProjection[A, B]]((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 compare a2
    case (Some(_), None) => GT
    case (None, Some(_)) => LT
    case (None, None) => EQ
  })

  import S._
  import MA._

  implicit def JavaIterableOrder[A](implicit oa: Order[A]): Order[java.lang.Iterable[A]] = IterableOrder(oa) <| (x => JavaIterableTo(x))
}
