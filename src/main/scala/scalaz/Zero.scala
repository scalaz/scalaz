package scalaz

trait Zero[+Z] {
  val zero: Z
}

object Zero {
  def z[Z](implicit x: Zero[Z]) = x.zero

  def zero[Z](z: Z) = new Zero[Z] {
    val zero = z
  }

  implicit val UnitZero = zero(())

  implicit val StringZero = zero("")
}
