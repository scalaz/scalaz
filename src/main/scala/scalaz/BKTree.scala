package scalaz

import collection.immutable.IntMap

sealed trait BKTree[+A] {
  import S._
  
  def isEmpty = this == BKTreeEmpty

  def size = this match {
    case BKTreeEmpty => 0
    case BKTreeNode(_, s, _) => s
  }

  def +[AA >: A](a: AA)(implicit m: MetricSpace[AA]): BKTree[AA] = this match {
    case BKTreeEmpty => BKTreeNode(a, 1, IntMap.empty)
    case BKTreeNode(v, s, c) => {
      val d = (v: AA) <===> a
      BKTreeNode(v, s + 1, c + ((d, c get d match {
        case None => BKTreeNode(a, 1, IntMap.empty)
        case Some(w) => w + a
      })))
    }
  }

  def -?-[AA >: A](a: AA)(implicit m: MetricSpace[AA]): Boolean = this match {
    case BKTreeEmpty => false
    case BKTreeNode(v, _, c) => {
      val d = (v: AA) <===> a
      d == 0 || (c get d match {
        case None => false
        case Some(w) => w -?- a
      })
    }
  }
}
private final case class BKTreeNode[+A](value: A, sz: Int, children: Map[Int, BKTree[A]]) extends BKTree[A]
private final case object BKTreeEmpty extends BKTree[Nothing]

object BKTree {
  def empty[A]: BKTree[A] = BKTreeEmpty 
}

/*
object T {
  def main(args: Array[String]) {
    import BKTree._
    val k = (empty[String] /: args)((a, b) => a + b)
    println(k)
    println(args.length == k.size)
    println(k -?- "abc")
  }
}
*/

