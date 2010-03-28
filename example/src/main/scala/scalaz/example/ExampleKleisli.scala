package scalaz.example

import scalaz._

object ExampleKleisli {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val f = ☆((n: Int) => if (n % 2 == 0) None else Some((n + 1).toString))
    val g = ☆((s: String) => if (List(5, 7) ∃ (_ == s.length)) None else Some("[" + s + "]"))

    // Kleisli composition 
    (List(7, 78, 98, 99, 100, 102, 998, 999, 10000) map (f >=> g apply _)) assert_≟ List(Some("[8]"), None, None, Some("[100]"), None, None, None, Some("[1000]"), None)
  }
}
