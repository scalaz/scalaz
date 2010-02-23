package scalaz

object ExampleFold {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Folding left over a List
    (List(1, 2, 3).foldl("0")((a, b) => "f(" + a.toString + ", " + b + ")")) assert_≟ "f(f(f(0, 1), 2), 3)"

    // Folding right over a List    
    (List(1, 2, 3).foldr("0")((a, b) => "f(" + a.toString + ", " + b + ")")) assert_≟ "f(1, f(2, f(3, 0)))"

    // Folding left over a List
    (List(1, 2, 3).foldl(0)(_ + _)) assert_≟ 6

    // Folding left over a List, using the first value as the starting value.
    (List(1, 2, 3) foldl1 (_ + _)) assert_≟ some(6)

    // Folding left over an empty List, using the first value as the starting value.
    (List.empty[Int] foldl1 (_ + _)) assert_≟ none

    // Summing over a List using FoldLeft, using the Int Monoid to combine the elements.
    (List(1, 2, 3) ∑) assert_≟ 6

    // Summing over a List using FoldLeft, using the String Monoid to combine the elements.
    (List("a", "b", "c") ∑) assert_≟ "abc"

    // Checking for existence of an element that satisfies a predicate.
    ("abC".toList ∃ (c => Character.isUpperCase(c))) assert_≟ true

    // Checking for existence of an element that satisfies a predicate, using Stream FoldRight,
    // which lazily handles the infinite Stream.
    (Stream.continually(1) ∃ (_ > 0)) assert_≟ true

    // Checking if all elements satisfy a predicate, using Stream FoldRight,
    // which lazily handles the infinite Stream.
    (Stream.continually(1) ∀ (_ == 0)) assert_≟ false

    // Counting the elements using Seq FoldRight
    ("123".toSeq ♯) assert_≟ 3

    // Counting the elements using Option FoldRight
    (Some(0) ♯) assert_≟ 1

    (50 ∈: Stream.range(0, 100)) assert_≟ true

    Stream.range(0, 100) ∋ 50 assert_≟ true
  }
}
