package scalaz

object ExampleArrow {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val plus1 = (_: Int) + 1

    // Applying first on the Function1 arrow.
    plus1.first apply (7, "abc") assert_≟ (8, "abc")

    // Applying second on the Function1 arrow.
    plus1.second apply ("def", 14) assert_≟ ("def", 15)
  }
}
