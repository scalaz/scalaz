package scalaz

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

  implicit def DigitEqual: Equal[Digit] = equalA

  implicit def OrderingEqual: Equal[Ordering]= equalA

  implicit def UnitEqual: Equal[Unit] = equalA

  implicit def StringEqual: Equal[String] = equalA

  implicit def IntEqual: Equal[Int] = equalA

  implicit def IntMultiplicationEqual: Equal[IntMultiplication] = IntEqual ∙ ((_: IntMultiplication).value)

  implicit def BooleanEqual: Equal[Boolean] = equalA

  implicit def BooleanConjunctionEqual: Equal[BooleanConjunction] = BooleanEqual ∙ ((_: BooleanConjunction).value)

  implicit def CharEqual: Equal[Char] = equalA

  implicit def CharMultiplicationEqual: Equal[CharMultiplication] = CharEqual ∙ ((_: CharMultiplication).value)

  implicit def ByteEqual: Equal[Byte] = equalA

  implicit def ByteMultiplicationEqual: Equal[ByteMultiplication] = ByteEqual ∙ ((_: ByteMultiplication).value)

  implicit def LongEqual: Equal[Long] = equalA

  implicit def LongMultiplicationEqual: Equal[LongMultiplication] = LongEqual ∙ ((_: LongMultiplication).value)

  implicit def ShortEqual: Equal[Short] = equalA

  implicit def ShortMultiplicationEqual: Equal[ShortMultiplication] = ShortEqual ∙ ((_: ShortMultiplication).value)

  implicit def FloatEqual: Equal[Float] = equalA

  implicit def DoubleEqual: Equal[Double] = equalA

  implicit def BigIntegerEqual: Equal[BigInteger] = equalA[java.math.BigInteger]

  implicit def BigIntegerMultiplicationEqual: Equal[BigIntegerMultiplication] = BigIntegerEqual ∙ ((_: BigIntegerMultiplication).value)

  implicit def BigIntEqual: Equal[BigInt] = equalA

  implicit def BigIntMultiplicationEqual: Equal[BigIntMultiplication] = BigIntEqual ∙ ((_: BigIntMultiplication).value)

  implicit def NodeSeqEqual: Equal[NodeSeq] = equalA

  implicit def NonEmptyListEqual[A](implicit ea: Equal[A]): Equal[NonEmptyList[A]] = IterableEqual(ea) ∙ ((_: NonEmptyList[A]).list)

  implicit def ZipStreamEqual[A](implicit ea: Equal[A]): Equal[ZipStream[A]] = IterableEqual(ea) ∙ ((_: ZipStream[A]).value)

  implicit def Tuple1Equal[A](implicit ea: Equal[A]): Equal[Tuple1[A]] = equal(_._1 ≟ _._1)

  implicit def Tuple2Equal[A, B](implicit ea: Equal[A], eb: Equal[B]): Equal[(A, B)] = equal {
    case ((a1, b1), (a2, b2)) => a1 ≟ a2 && b1 ≟ b2
  }

  implicit def Tuple3Equal[A, B, C](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C]): Equal[(A, B, C)] = equal {
    case ((a1, b1, c1), (a2, b2, c2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2
  }

  implicit def Tuple4Equal[A, B, C, D](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D]): Equal[(A, B, C, D)] = equal {
    case ((a1, b1, c1, d1), (a2, b2, c2, d2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2
  }

  implicit def Tuple5Equal[A, B, C, D, E](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E]): Equal[(A, B, C, D, E)] = equal {
    case ((a1, b1, c1, d1, e1), (a2, b2, c2, d2, e2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2
  }

  implicit def Tuple6Equal[A, B, C, D, E, F](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F]): Equal[(A, B, C, D, E, F)] = equal {
    case ((a1, b1, c1, d1, e1, f1), (a2, b2, c2, d2, e2, f2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2 && f1 ≟ f2
  }

  implicit def Tuple7Equal[A, B, C, D, E, F, G](implicit ea: Equal[A], eb: Equal[B], ec: Equal[C], ed: Equal[D], ee: Equal[E], ef: Equal[F], eg: Equal[G]): Equal[(A, B, C, D, E, F, G)] = equal {
    case ((a1, b1, c1, d1, e1, f1, g1), (a2, b2, c2, d2, e2, f2, g2)) => a1 ≟ a2 && b1 ≟ b2 && c1 ≟ c2 && d1 ≟ d2 && e1 ≟ e2 && f1 ≟ f2 && g1 ≟ g2
  }                             

  implicit def Function0Equal[A](implicit ea: Equal[A]): Equal[Function0[A]] = equal(_.apply ≟ _.apply)

  implicit def OptionEqual[A](implicit ea: Equal[A]): Equal[Option[A]] = equal {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  }

  implicit def EitherEqual[A, B](implicit ea: Equal[A], eb: Equal[B]): Equal[Either[A, B]] = equal {
    case (Left(a1), Left(a2)) => a1 ≟ a2
    case (Right(b1), Right(b2)) => b1 ≟ b2
    case _ => false
  }

  implicit def EitherLeftEqual[A, X](implicit ea: Equal[A]): Equal[Either.LeftProjection[A, X]] = equal((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  })

  implicit def EitherRightEqual[X, A](implicit ea: Equal[A]): Equal[Either.RightProjection[X, A]] = equal((a1, a2) => (a1.toOption, a2.toOption) match {
    case (Some(a1), Some(a2)) => a1 ≟ a2
    case (a1, a2) => a1.isDefined == a2.isDefined
  })

  implicit def ValidationEqual[E, A](implicit ee: Equal[E], ea: Equal[A]): Equal[Validation[E, A]] = EitherEqual(ee, ea) ∙ ((_: Validation[E, A]).either)

  implicit def TreeEqual[A](implicit ea: Equal[A]): Equal[Tree[A]] =
    equal[Tree[A]]((a1, a2) => a1.rootLabel ≟ a2.rootLabel
        && IterableEqual[Tree[A]].equal(a1.subForest, a2.subForest))

  implicit def TreeLocEqual[A](implicit ea: Equal[A]): Equal[TreeLoc[A]] = {
    equal[TreeLoc[A]]((a1, a2) => a1.tree ≟ a2.tree
        && a1.lefts ≟ a2.lefts && a1.rights ≟ a2.rights && a1.parents ≟ a2.parents)
  }

  import concurrent.Promise
  implicit def PromiseEqual[A](implicit ea: Equal[A]): Equal[Promise[A]] =
    equal[Promise[A]]((a1, a2) => a1.get ≟ a2.get)

  implicit def IterableEqual[A](implicit ea: Equal[A]): Equal[Iterable[A]] = equal((a1, a2) => {
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

  implicit def JavaIterableEqual[A](implicit ea: Equal[A]): Equal[java.lang.Iterable[A]] = equal((a1, a2) => {
    import Scalaz._

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

  implicit def JavaMapEntry[K, V](implicit ek: Equal[K], ev: Equal[V]): Equal[java.util.Map.Entry[K, V]] = equal((a1, a2) => a1.getKey ≟ a2.getKey)

  implicit def JavaMapEqual[K, V](implicit ek: Equal[K], ev: Equal[V]): Equal[java.util.Map[K, V]] = equal(_.entrySet ≟ _.entrySet)
}
