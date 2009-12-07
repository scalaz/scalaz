package scalaz

object ExamplePlus {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    List(1, 2, 3) ⟴ List(4, 5, 6) assert_≟ List(1, 2, 3, 4, 5, 6)
    1 ➝: 2 ➝: 3 ➝: List(4, 5, 6) assert_≟ List(1, 2, 3, 4, 5, 6)
  }
}