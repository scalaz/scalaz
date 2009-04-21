package scalaz

sealed trait CharW {
  val value: Char

  def digit = Digit.digits find (_.toChar == value)

  def |*| = CharMultiplication.multiplication(value)  
}

object CharW {
  implicit def CharTo(c: Char) = new CharW {
    val value = c
  }

  implicit def CharFrom(c: CharW) = c.value
}