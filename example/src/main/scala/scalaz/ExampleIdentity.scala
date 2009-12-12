package scalaz

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
  }
}
