package scalaz

import collection.immutable.Stream


object ExampleTree {
  def main(args: Array[String]) = run

  import Scalaz._

  def run {
    val tree: Tree[Int] = node(1, Stream(leaf(2), node(3, Stream(leaf(4)))))
    leafPaths(tree).toList.map(_.toList.reverse)  assert_≟ List(List(1, 2), List(1, 3, 4))
  }

  def leafPaths[T](tree: Tree[T]): Stream[Stream[T]]
      = tree.loc.cojoin.toTree.flatten.filter(_.isLeaf).map(_.path)
}