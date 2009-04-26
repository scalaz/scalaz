package scalaz

import collection.immutable.IntMap

// http://hackage.haskell.org/packages/archive/bktrees/0.2.1/doc/html/src/Data-Set-BKTree.html

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

  def =?=[AA >: A](a: AA, n: Int)(implicit m: MetricSpace[AA]): Boolean = this match {
    case BKTreeEmpty => false
    case BKTreeNode(v, _, c) => {
      val d = (v: AA) <===> a
      d <= n || (splitMap(splitMap(c, d - n - 1)._2, d + n + 1)._1 exists (_._2 =?= (a, n)))
    }
  }

  private def splitChildren[AA >: A](k: Int): (Map[Int, BKTree[AA]], Map[Int, BKTree[AA]]) = this match {
    case BKTreeEmpty => (IntMap.empty, IntMap.empty)
    case BKTreeNode(_, _, c) => splitMap(c, k)
  }

  private def splitMap[AA >: A](m: Map[Int, BKTree[AA]], k: Int): (Map[Int, BKTree[AA]], Map[Int, BKTree[AA]]) = {
    var m1: Map[Int, BKTree[AA]] = IntMap.empty
    var m2: Map[Int, BKTree[AA]] = IntMap.empty
    for((i, v) <- m.elements) {
      if(i < k)
        m1 = m1 + ((i, v))
      else if(i > k)
        m2 = m2 + ((i, v))
    }
    (m1, m2)
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

