package scalaz

sealed trait ShortMultiplication extends NewType[Short]

trait ShortMultiplications {
  def multiplication(n: Short): ShortMultiplication = new ShortMultiplication {
    val value = n
  }
}
