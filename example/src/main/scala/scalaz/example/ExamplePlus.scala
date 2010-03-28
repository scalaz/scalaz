package scalaz.example

import scalaz._

object ExamplePlus {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Using the List Plus for appending
    List(1, 2, 3) <+> List(4, 5, 6) assert_≟ List(1, 2, 3, 4, 5, 6)

    // Prepended elements with List Pure then Plus 
    1 <+>: 2 <+>: 3 <+>: List(4, 5, 6) assert_≟ List(1, 2, 3, 4, 5, 6)
  }
}