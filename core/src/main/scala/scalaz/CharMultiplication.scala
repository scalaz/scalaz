package scalaz

sealed trait CharMultiplication extends NewType[Char]

trait CharMultiplications {
  def multiplication(n: Char): CharMultiplication = new CharMultiplication {
    val value = n
  }
}
