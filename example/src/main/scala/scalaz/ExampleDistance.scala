package scalaz

// Demonstrates the <---> method to compute edit distance using a metric space such as levenshtein
object ExampleDistance {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val j = "algorithm" toArray
    val k = "altruistic" toArray

    j <---> k assert_≟ 6
  }
}
