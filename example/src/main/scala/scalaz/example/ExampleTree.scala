package scalaz.example

import scalaz._

import collection.immutable.Stream


object ExampleTree {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val tree: Tree[Int] =
    1.node(
      2.leaf,
      3.node(
        4.leaf))

    // Tree is a Pointed Functor...
    1.η[Tree] assert_≟ 1.leaf
    tree ∘ (1 +) assert_≟ 2.node(3.leaf, 4.node(5.leaf))

    // ...and a Monad
    val t2 = tree ∗ (x => (x == 2) ? x.leaf | x.node((-x).leaf))
    t2 assert_≟ 1.node((-1).leaf, 2.leaf, 3.node((-3).leaf, 4.node((-4).leaf)))

    // ...and Traversable
    tree.collapse assert_≟ 10

    // ...and Foldable
    tree.foldMap(_.toString) assert_≟ "1234"

    // A tree of TreeLocs (aka Zipper). Each TreeLoc is rooted at `tree` but focussed on a different node.
    val allTreeLocs: Tree[TreeLoc[Int]] = tree.loc.cojoin.toTree
    // Getting the label of the focussed node from each TreeLoc restores the original tree
    allTreeLocs.map(_.getLabel) assert_≟ tree
    // Alternatively, we can get the path to root from each node
    allTreeLocs.map(_.path).drawTree.println

    // And finally wrap this up as a function:
    leafPaths(tree).toList.map(_.toList.reverse) assert_≟ List(List(1, 2), List(1, 3, 4))
  }

  /**
   * Returns the paths from each leaf node back to the root node.
   */
  def leafPaths[T](tree: Tree[T]): Stream[Stream[T]]
  = tree.loc.cojoin.toTree.flatten.filter(_.isLeaf).map(_.path)
}