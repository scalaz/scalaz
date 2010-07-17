package scalaz.example

import scalaz._
import Digit._

object ExampleTraverse {
  def main(args: Array[String]) = run

  import Scalaz._
  import collection.mutable.ArraySeq

  def run {
    // Sequence the List with the Option applicative functor
    List(some(7), some(9)).sequence assert_≟ some(List(7, 9))

    // Sequence the Stream with the Option applicative functor
    Stream(Some(7), None, Some(9)).sequence assert_≟ none

    val f = (_: String).map(_ - 48).toList
    def g(s: String): FirstOption[Int] = {
      val validation = s.parseInt
      val option = validation.either.right.toOption
      val fst = option.fst
      fst
    }

    // Traverse the List with the FirstOption applicative functor (domain of g)
    (List("abc", "def") ↦ g).value assert_≟ none

    // Traverse the List with the FirstOption applicative functor (domain of g)
    (List("7", "8") ↦ g).value assert_≟ some(List(7, 8))

    // Traverse the Option with the FirstOption applicative functor (domain of g)
    (some("abc") ↦ g).value assert_≟ none

    // Traverse the Option with the FirstOption applicative functor (domain of g)
    (some("9") ↦ g).value assert_≟ some(some(9))

    // Traverse a List of characters to get a possible List of digits (scalaz.Digit) using the Option applicative functor
    List('1', '2', '3').traverseDigits assert_≟ some(List(_1, _2, _3))

    // Traverse an Option of characters to get a possible Option of digits (scalaz.Digit) using the Option applicative functor
    some('1').traverseDigits assert_≟ some(some(_1))

    // Traverse a ArraySeq of characters to get a possible ArraySeq of digits (scalaz.Digit) using the Option applicative functor
    ArraySeq('1', 'x', '3').traverseDigits assert_≟ none

    // Traverse a List using the String monoid
    List(100, 200, 300) ↣ (_.toString) assert_≟ "100200300"

    // Traverse a ArraySeq using the Int addition monoid
    ArraySeq(100, 200, 300) ↣ (x => x) assert_≟ 600

    // Traverse a Stream using the Int multiplication monoid
    (Stream(100, 200, 300) ↣ (x => x ∏)).value assert_≟ 6000000

    // Traverse an Option using the Int multiplication monoid
    (some(100) ↣ (x => x ∏)).value assert_≟ 100

    // Traverse an Option using the Int multiplication monoid
    (none[Long] ↣ (x => x ∏)).value assert_≟ 1L

    // Traverse (collapse) a List using the Int addition monoid    
    List(100, 200, 300).collapse assert_≟ 600
  }
}
