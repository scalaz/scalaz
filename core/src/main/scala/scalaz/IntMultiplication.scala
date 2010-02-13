package scalaz

sealed trait IntMultiplication extends NewType[Int]

trait IntMultiplications{
  def multiplication(n: Int): IntMultiplication = new IntMultiplication {
    val value = n
  }
}
