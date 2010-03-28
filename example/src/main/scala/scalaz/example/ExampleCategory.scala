package scalaz.example

import scalaz._

object ExampleCategory {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val f = (x: Int) => (x * 7).toString
    val g = (s: String) => s.reverse.toInt

    // Right composition of the Function1 category
    (f ⋙ g apply 33) assert_≟ 132

    // Left composition of the Function1 category
    (g ⋘ f apply 33) assert_≟ 132
  }
}