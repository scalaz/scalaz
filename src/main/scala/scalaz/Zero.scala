package scalaz

trait Zero[+Z] {
  val zero: Z
}

object Zero {
  def z[Z](implicit x: Zero[Z]) = x.zero

  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  implicit val OrderingZero = zero(EQ)

  implicit val UnitZero = zero(())

  implicit val StringZero = zero("")

  implicit val IntZero = zero(0)

  implicit val BooleanZero = zero(false)

  implicit val CharZero = zero(0.toChar)

  implicit val ByteZero = zero(0.toByte)

  implicit val LongZero = zero(0L)

  implicit val ShortZero = zero(0.toShort)

  implicit val FloatZero = zero(0F)

  implicit val DoubleZero = zero(0D)
}
