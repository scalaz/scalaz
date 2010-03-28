package scalaz.example

import scalaz._

// Demonstrates the <---> method to compute edit distance using a metric space such as levenshtein
object ExampleDistance {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val j = "algorithm" toArray
    val k = "altruistic" toArray

    // Compute the edit distance between "algorithm" and "altruistic"
    j <---> k assert_≟ 6

    val x = 1638452297 digits
    val y = 444488444 digits

    // Compute the edit distance between two strings of digits
    x <---> y assert_≟ 9
  }
}
