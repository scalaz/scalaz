package scalaz

object ExampleMonoid {
  def main(args: Array[String]) = run

  import scalaz.Scalaz._

  def run {
    "abc" ⊹ "def" assert_≟ "abcdef"

    7 ⊹ 8 assert_≟ 15

    (7 ∏) ⊹ (8 ∏) assert_≟ (56 ∏)

    true ⊹ true assert_≟ true
    true ⊹ false assert_≟ true
    false ⊹ true assert_≟ true
    false ⊹ false assert_≟ false

    (true |∧|) ⊹ (true |∧|) assert_≟ (true |∧|)
    (true |∧|) ⊹ (false |∧|) assert_≟ (false |∧|)
    (false |∧|) ⊹ (true |∧|) assert_≟ (false |∧|)
    (false |∧|) ⊹ (false |∧|) assert_≟ (false |∧|)

    {
      val f = (n: Int) => "AAA" + n.toString + "ZZZ"
      val g = (n: Int) => n.toString.reverse.mkString
      (f ⊹ g).apply(123) assert_≟ "AAA123ZZZ321"
    }

    List(1, 2, 3) ⊹ List(4, 5, 6) assert_≟ List(1, 2, 3, 4, 5, 6)

    // Zero of the List Semigroup
    ∅[List[Int]] assert_≟ List.empty[Int]

    // Zero of the Int Semigroup
    ∅[Int] assert_≟ 0

    // Zero of the IntMultiplication Semigroup
    ∅[IntMultiplication] assert_≟ (1 ∏)

    // Appending zero must leave t unchanged
    def assertIdentity[T](t: T)(implicit st: Monoid[T], eq: Equal[T], sh: Show[T]) = (t ⊹ ∅[T]) assert_≟ (t)
    assertIdentity(List(1))
    assertIdentity(1)
    assertIdentity(1 ∏)
  }
}