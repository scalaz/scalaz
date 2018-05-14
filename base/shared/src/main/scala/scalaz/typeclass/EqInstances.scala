package scalaz
package typeclass

trait EqInstances {
  import java.lang.Double.doubleToRawLongBits
  import java.lang.Float.floatToRawIntBits

  private[this] def singletonEq[A]: Eq[A] = instanceOf[EqClass[A]]((a, b) => true)
  private[this] def universalEq[A]: Eq[A] = instanceOf[EqClass[A]]((a, b) => a == b)

  implicit final val unitEq: Eq[Unit] = singletonEq[Unit]

  implicit final val boolEq: Eq[Boolean] = universalEq[Boolean]
  implicit final val byteEq: Eq[Byte]    = universalEq[Byte]
  implicit final val shortEq: Eq[Short]  = universalEq[Short]
  implicit final val intEq: Eq[Int]      = universalEq[Int]
  implicit final val longEq: Eq[Long]    = universalEq[Long]

  implicit final val floatEq: Eq[Float] =
    instanceOf[EqClass[Float]]((a, b) => floatToRawIntBits(a) == floatToRawIntBits(b))

  implicit final val doubleEq: Eq[Double] =
    instanceOf[EqClass[Double]]((a, b) => doubleToRawLongBits(a) == doubleToRawLongBits(b))

  implicit final def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
    instanceOf[EqClass[Option[A]]] {
      case (None, None)         => true
      case (Some(a1), Some(a2)) => A.equal(a1, a2)
      case _                    => false
    }

  implicit final def eitherEq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Either[A, B]] =
    instanceOf[EqClass[Either[A, B]]] {
      case (Left(a1), Left(a2))   => A.equal(a1, a2)
      case (Right(b1), Right(b2)) => B.equal(b1, b2)
      case _                      => false
    }

  implicit final def tuple1Eq[A](implicit A: Eq[A]): Eq[Tuple1[A]] =
    instanceOf[EqClass[Tuple1[A]]] {
      case (Tuple1(a1), Tuple1(a2)) => A.equal(a1, a2)
      case _                        => false
    }

  implicit final def tuple2Eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Tuple2[A, B]] =
    instanceOf[EqClass[Tuple2[A, B]]] {
      case ((a1, b1), (a2, b2)) => A.equal(a1, a2) && B.equal(b1, b2)
      case _                    => false
    }

  implicit final def tuple3Eq[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A, B, C)] =
    instanceOf[EqClass[(A, B, C)]] {
      case ((a1, b1, c1), (a2, b2, c2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2)
      case _ => false
    }

  implicit final def tuple4Eq[A, B, C, D](implicit A: Eq[A], B: Eq[B], C: Eq[C], D: Eq[D]): Eq[(A, B, C, D)] =
    instanceOf[EqClass[(A, B, C, D)]] {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2) && D.equal(d1, d2)
      case _ => false
    }
}
