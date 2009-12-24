package scalaz

sealed trait IntMultiplication extends NewType[Int]

trait IntMultiplications {
  def multiplication(n: Int) = new IntMultiplication {
    val value = n
  }
}
