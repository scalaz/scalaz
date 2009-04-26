package scalaz

import collection.immutable.IntMap

sealed trait BKTree[+A] {
  val value: A
  val children: Map[Int, BKTree[A]]

  import S._

  def +[AA >: A](a: AA)(implicit m: MetricSpace[AA]): BKTree[AA] = {
    val d = (value: AA) <===> a

    val j = children + ((d, children get d match {
      case None => a.bktree
      case Some(cs) => cs + a
    }))

    BKTree.bktree(a, j)
  }
}

object BKTree {
  def bktree[A](v: A): BKTree[A] = bktree(v, IntMap.empty)

  private def bktree[A](v: A, c: Map[Int, BKTree[A]]): BKTree[A] = new BKTree[A] {
    val value = v
    val children = c
  }
}