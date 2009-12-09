package scalaz

object ExampleSemigroup {
  def main(args: Array[String]) = run

  import Scalaz._

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
      println((f ⊹ g) apply 123)
    }

    List(1, 2, 3) ⊹ List(4, 5, 6) assert_≟ List(1, 2, 3, 4, 5, 6)
  }
}
