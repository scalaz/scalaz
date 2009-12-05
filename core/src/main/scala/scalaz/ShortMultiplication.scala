package scalaz

sealed trait ShortMultiplication {
  val value: Short
}

trait ShortMultiplications {
  def multiplication(n: Short) = new ShortMultiplication {
    val value = n
  }

  implicit def ShortMultiplicationFrom(n: ShortMultiplication): Short = n.value
}
