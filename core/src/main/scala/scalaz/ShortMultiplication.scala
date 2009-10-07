package scalaz

sealed trait ShortMultiplication {
  val value: Short
}

object ShortMultiplication {
  def multiplication(n: Short) = new ShortMultiplication {
    val value = n
  }

  implicit def ShortMultiplicationFrom(n: ShortMultiplication) = n.value
}
