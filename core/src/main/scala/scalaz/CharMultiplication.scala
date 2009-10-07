package scalaz

sealed trait CharMultiplication {
  val value: Char
}

object CharMultiplication {
  def multiplication(n: Char) = new CharMultiplication {
    val value = n
  }

  implicit def CharMultiplicationFrom(n: CharMultiplication) = n.value
}
