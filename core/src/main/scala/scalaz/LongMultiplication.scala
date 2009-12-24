package scalaz

sealed trait LongMultiplication extends NewType[Long]

trait LongMultiplications {
  def multiplication(n: Long) = new LongMultiplication {
    val value = n
  }
}
