package scalaz.example

import scalaz._

object ExampleFunctor {
  def main(args: Array[String]) = run

  import Scalaz._
  
  def run {
    // Map across the List functor
    (List(1, 2, 3, 4, 5) ∘ (1 +)) assert_≟ List(2, 3, 4, 5, 6)

    // Map across the Option functor
    (some(7) ∘ (1 +)) assert_≟ some(8)

    // Map across the Function1 functor
    val len = (s: String) => s.length
    (len ∘ (1 + (_: Int))).apply("foo") assert_≟ 4

    // Map across the Option functor within a map across the List functor 
    (List(Some(7), None, Some(8)) ∘∘ (1 + (_: Int))) assert_≟ List(Some(8), None, Some(9))
  }
}
