package scalaz

sealed trait IntMultiplication {
  val value: Int
}

trait IntMultiplications {
  def multiplication(n: Int) = new IntMultiplication {
    val value = n
  }

  implicit def IntMultiplicationFrom(n: IntMultiplication): Int = n.value
}
