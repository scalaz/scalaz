package scalaz

import collection.immutable.IntMap

sealed trait BKTree[+A] {
  val root: A
  val children: IntMap[A]
}

object BKTree {
  def bktree[A](r: A) = new BKTree[A] {
    val root = r
    val children = IntMap.empty
  }
}