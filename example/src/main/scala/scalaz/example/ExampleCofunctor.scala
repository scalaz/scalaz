package scalaz.example

import scalaz._

object ExampleCofunctor {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Contravariant functor map
    {
      val plus3 = (3 + (_: Int))
      val div2 = (_: Int) / 2
      (List(1, 2, 3, 4, 5) ∘ (plus3 ∙ div2)) assert_≟ List(3, 4, 4, 5, 5)
    }
  }
}