package scalaz

sealed trait CharMultiplication {
  val value: Char
}

trait CharMultiplications {
  def multiplication(n: Char) = new CharMultiplication {
    val value = n
  }

  implicit def CharMultiplicationFrom(n: CharMultiplication): Char = n.value
}
