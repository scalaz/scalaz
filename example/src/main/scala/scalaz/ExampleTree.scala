package scalaz

import collection.immutable.Stream


object ExampleTree {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val tree: Tree[Int] = node(1, Stream(leaf(2), node(3, Stream(leaf(4)))))

    // A tree of TreeLocs (aka Zipper). Each TreeLoc is rooted at `tree` but focussed on a different node.
    val allTreeLocs: Tree[TreeLoc[Int]] = tree.loc.cojoin.toTree
    // Getting the label of the focussed node from each TreeLoc restores the original tree
    allTreeLocs.map(_.getLabel) assert_≟ tree
    // Alternatively, we can get the path to root from each node
    allTreeLocs.map(_.path).drawTree.println

    // And finally wrap this up as a function:
    leafPaths(tree).toList.map(_.toList.reverse)  assert_≟ List(List(1, 2), List(1, 3, 4))
  }

  /**
   * Returns the paths from each leaf node back to the root node.
   */
  def leafPaths[T](tree: Tree[T]): Stream[Stream[T]]
      = tree.loc.cojoin.toTree.flatten.filter(_.isLeaf).map(_.path)
}