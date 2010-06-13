package scalaz.example

import scalaz._

import java.lang.String

object ExampleIdentity {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    // Any value can be converted to Identity
    1: Identity[Int]

    // Wrap a value in NonEmptyList
    1.wrapNel assert_≟ NonEmptyList(1)

    // Unfold a value into a Tree, by recursively applying a function to generate the child nodes
    // from their parent.
    val tree: Tree[String] = 1.unfoldTree[String] {
      (a: Int) => (a.toString, () => (List(1, 2).map(_ + a).toStream.filter(_ < 4): Stream[Int]))
    }
    tree assert_≟ node("1", Stream(node("2", Stream(leaf("3"))), leaf("3")))

    // Starting with the initial value `n`, iterate a function (Int => Int) through the Stream Pure and Stream Monoid
    // to generate the Collatz sequence. In homage to http://xkcd.com/710/
    def collatz(n: Int) = n.iterate[Stream](a => (a % 2 == 0) ? (a/2) | 3*a + 1).takeWhile(_ > 1)
    collatz(7).toList assert_≟ List(7, 22, 11, 34, 17, 52, 26, 13, 40, 20, 10, 5, 16, 8, 4, 2)

    // Elvis operator
    (null: String) ?? "default" assert_≟ "default"

    1 matchOrZero { case x: Int => x * 2 } assert_≟ 2
    
    1 matchOrZero { case x: Int if false => x * 2 } assert_≟ 0
  }
}
