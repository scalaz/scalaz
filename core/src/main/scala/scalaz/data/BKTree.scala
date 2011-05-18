package scalaz

import collection.immutable.IntMap
import annotation.tailrec

// http://hackage.haskell.org/packages/archive/bktrees/0.2.1/doc/html/src/Data-Set-BKTree.html

sealed trait BKTree[+A] {
  import Scalaz._

  def isEmpty : Boolean = this == BKTreeEmpty

  def size : Int = this match {
    case BKTreeEmpty => 0
    case BKTreeNode(_, s, _) => s
  }

  def +[AA >: A : MetricSpace](a: AA): BKTree[AA] = this match {
    case BKTreeEmpty => BKTreeNode(a, 1, IntMap.empty)
    case BKTreeNode(v, s, c) => {
      val d = (v: AA) <===> a
      BKTreeNode(v, s + 1, c + ((d, c get d match {
        case None => BKTreeNode(a, 1, IntMap.empty)
        case Some(w) => w + a
      })))
    }
  }

  def ++[AA >: A: MetricSpace](t: BKTree[AA]): BKTree[AA] = {
    var k: BKTree[AA] = this
    for(v <- t.values)
      k = k + v
    k
  }

  def values: List[A] = this match {
    case BKTreeEmpty => Nil
    case BKTreeNode(v, _, c) => v :: c.valuesIterator.toList.flatMap(_.values)
  }

  @tailrec
  final def -?-[AA >: A: MetricSpace](a: AA): Boolean = this match {
    case BKTreeEmpty => false
    case BKTreeNode(v, _, c) => {
      val d = (v: AA) <===> a
      d == 0 || (c get d match {
        case None => false
        case Some(w) => w -?- a
      })
    }
  }

  def =?=[AA >: A: MetricSpace](a: AA, n: Int): Boolean = this match {
    case BKTreeEmpty => false
    case BKTreeNode(v, _, c) => {
      val d = (v: AA) <===> a
      d <= n || (subChildren(d, n) exists (_._2 =?= (a, n)))
    }
  }

  def |=|[AA >: A: MetricSpace](a: AA, n: Int): List[AA] = this match {
    case BKTreeEmpty => Nil
    case BKTreeNode(v, _, c) => {
      val d = (v: AA) <===> a
      val k = subChildren(d, n).valuesIterator.toList flatMap (_ |=| (a, n))
      if(d <= n)
        v :: k
      else
        k
    }
  }

  private def subChildren[AA >: A](d: Int, n: Int) = this match {
    case BKTreeEmpty => IntMap.empty
    case BKTreeNode(_, _, c) => subMap(c, d, n)
  }

  private def subMap[AA >: A](m: Map[Int, BKTree[AA]], d: Int, n: Int) = splitMap(splitMap(m, d - n - 1)._2, d + n + 1)._1

  private def splitChildren[AA >: A](k: Int): (Map[Int, BKTree[AA]], Map[Int, BKTree[AA]]) = this match {
    case BKTreeEmpty => (IntMap.empty, IntMap.empty)
    case BKTreeNode(_, _, c) => splitMap(c, k)
  }

  private def splitMap[AA >: A](m: Map[Int, BKTree[AA]], k: Int): (Map[Int, BKTree[AA]], Map[Int, BKTree[AA]]) = {
    var m1: Map[Int, BKTree[AA]] = IntMap.empty
    var m2: Map[Int, BKTree[AA]] = IntMap.empty
    for((i, v) <- m.iterator) {
      if(i < k)
        m1 = m1 + ((i, v))
      else if(i > k)
        m2 = m2 + ((i, v))
    }
    (m1, m2)
  }
}
private final case class BKTreeNode[+A](value: A, sz: Int, children: Map[Int, BKTree[A]]) extends BKTree[A]
private case object BKTreeEmpty extends BKTree[Nothing]

trait BKTrees {
  def emptyBKTree[A]: BKTree[A] = BKTreeEmpty
}
