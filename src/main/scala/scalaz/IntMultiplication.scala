package scalaz

sealed trait IntMultiplication {
  val value: Int
}

object IntMultiplication {
  def multiplication(n: Int) = new IntMultiplication {
    val value = n
  }

  implicit def IntMultiplicationFrom(n: IntMultiplication) = n.value
}
