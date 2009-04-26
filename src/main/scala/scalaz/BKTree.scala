package scalaz

import collection.immutable.IntMap

// todo broken
// http://hackage.haskell.org/packages/archive/bktrees/0.2.1/doc/html/src/Data-Set-BKTree.html

sealed trait BKTree[+A] {
  def isEmpty = this == BKTreeEmpty

  def size = this match {
    case BKTreeEmpty => 0
    case BKTreeNode(_, s, _) => s
  }
}
private final case class BKTreeNode[+A](value: A, sz: Int, children: IntMap[BKTree[A]]) extends BKTree[A]
private final case object BKTreeEmpty extends BKTree[Nothing]

object BKTree {
  
}
