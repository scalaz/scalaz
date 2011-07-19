package scalaz.example

import scalaz._

object ExampleState {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    treeLabel
  }

  /**
   * See <a href="http://blog.tmorris.net/the-state-monad-for-scala-users">The State Monad for Scala users</a>
   */
  def treeLabel {
    sealed abstract class Tree[A] {
      /**
       * Label the Leaf nodes of a the tree with increasing integers, traversing
       * left to right. The current value of the label is be explicitly threaded
       * through the recursion. 
       */
      def number(seed: Int): (Tree[(A, Int)], Int) = this match {
        case Leaf(x) => (Leaf(x, seed), seed + 1)
        case Branch(left, right) => left number seed match {
          case (l, ls) => {
            right number ls match {
              case (r, rs) => (Branch(l, r), rs)
            }
          }
        }
      }

      /**
       * Use the State Monad to implicitly thread the current value
       * of the label through the recursion.
       */
      def numberSM: State[Int, Tree[(A, Int)]] = this match {
        case Leaf(x) => for {s <- get[Int];
                             _ <- modify((_: Int) + 1)} yield Leaf((x, s))
        case Branch(left, right) => for {l <- left.numberSM
                                         r <- right.numberSM} yield Branch(l, r)
      }
    }

    final case class Leaf[A](a: A) extends Tree[A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

    implicit def TreeShow[X] = showA[Tree[X]]
    implicit def TreeEqual[X] = equalA[Tree[X]]

    val tree = Branch(Leaf("one"), Branch(Leaf("two"), Leaf("three")))
    tree.number(1)._1 assert_=== Branch(Leaf(("one", 1)), Branch(Leaf(("two", 2)), Leaf(("three", 3))))
    tree.numberSM eval 1 assert_=== tree.number(1)._1
  }
}