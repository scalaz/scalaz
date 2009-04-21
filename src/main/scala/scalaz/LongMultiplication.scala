package scalaz

sealed trait LongMultiplication {
  val value: Long
}

object LongMultiplication {
  def multiplication(n: Long) = new LongMultiplication {
    val value = n
  }

  implicit def LongMultiplicationFrom(n: LongMultiplication) = n.value
}
