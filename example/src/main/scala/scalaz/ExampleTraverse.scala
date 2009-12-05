package scalaz

object ExampleTraverse {
  def main(args: Array[String]) = run

  import Scalaz._
  import collection.mutable.GenericArray

  def run {
    // Sequence the List with the Option applicative functor
    List(some(7), some(9)).sequence assert_≟ some(List(7, 9))

    // Sequence the Stream with the Option applicative functor
    Stream(Some(7), None, Some(9)).sequence assert_≟ none

    val f = (_: String).map(_ - 48).toList
    def g(s: String): Option[Int] = s.parseInt.either.right.toOption

    // Traverse the List with the Option applicative functor (domain of g)
    (List("abc", "def") ↦ g) assert_≟ none

    // Traverse the List with the Option applicative functor (domain of g)
    (List("7", "8") ↦ g) assert_≟ some(List(7, 8))

    // Traverse the Option with the Option applicative functor (domain of g)
    (some("abc") ↦ g) assert_≟ none

    // Traverse the Option with the Option applicative functor (domain of g)
    (some("9") ↦ g) assert_≟ some(some(9))

    // Traverse a List of characters to get a possible List of digits (scalaz.Digit) using the Option applicative functor
    List('1', '2', '3').traverseDigits assert_≟ some(List(_1, _2, _3))

    // Traverse an Option of characters to get a possible Option of digits (scalaz.Digit) using the Option applicative functor
    some('1').traverseDigits assert_≟ some(some(_1))

    // Traverse a GenericArray of characters to get a possible GenericArray of digits (scalaz.Digit) using the Option applicative functor
    GenericArray('1', 'x', '3').traverseDigits assert_≟ none

    // Traverse a List using the String monoid
    List(100, 200, 300) ↣ (_.toString) assert_≟ "100200300"

    // Traverse a GenericArray using the Int addition monoid
    GenericArray(100, 200, 300) ↣ (x => x) assert_≟ 600
  
    // Traverse a Stream using the Int multiplication monoid
    (Stream(100, 200, 300) ↣ (x => x ∏)).value assert_≟ 6000000

    // Traverse an Option using the Int multiplication monoid
    (some(100) ↣ (x => x ∏)).value assert_≟ 100

    // Traverse an Option using the Int multiplication monoid
    (none[Long] ↣ (x => x ∏)).value assert_≟ 1L

    // Traverse (collapse) a List using the Int addition monoid    
    List(100, 200, 300).collapse.value assert_≟ 600
  }
}
