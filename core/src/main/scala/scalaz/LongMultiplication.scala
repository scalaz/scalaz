package scalaz

sealed trait LongMultiplication {
  val value: Long
}

trait LongMultiplications {
  def multiplication(n: Long) = new LongMultiplication {
    val value = n
  }

  implicit def LongMultiplicationFrom(n: LongMultiplication): Long = n.value
}
