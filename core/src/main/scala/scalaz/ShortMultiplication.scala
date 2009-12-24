package scalaz

sealed trait ShortMultiplication extends NewType[Short]

trait ShortMultiplications {
  def multiplication(n: Short) = new ShortMultiplication {
    val value = n
  }
}
