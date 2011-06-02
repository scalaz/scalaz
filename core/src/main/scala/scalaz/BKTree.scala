package scalaz

import collection.immutable.IntMap
import annotation.tailrec

// http://hackage.haskell.org/packages/archive/bktrees/0.2.1/doc/html/src/Data-Set-BKTree.html

sealed trait BKTree[A] {
  def isEmpty: Boolean =
    this == BKTreeEmpty

  // private case class BKTreeNode[A](value: A, sz: Int, children: Map[Int, BKTree[A]]) extends BKTree[A]

  def map[B](f: A => B): BKTree[B] =
    this match {
      case BKTreeEmpty() => BKTreeEmpty()
      case BKTreeNode(a, s, c) => BKTreeNode(f(a), s, c.mapValues(_ map f))
    }

  def size: Int =
    this match {
      case BKTreeEmpty() => 0
      case BKTreeNode(_, s, _) => s
    }

  def +(a: A)(implicit m: MetricSpace[A]): BKTree[A] =
    this match {
      case BKTreeEmpty() => BKTreeNode(a, 1, IntMap.empty)
      case BKTreeNode(v, s, c) => {
        val d = m.distance(v)(a)
        BKTreeNode(v, s + 1, c + ((d, c get d match {
          case None => BKTreeNode(a, 1, IntMap.empty)
          case Some(w) => w + a
        })))
      }
    }

  def ++(t: BKTree[A])(implicit m: MetricSpace[A]): BKTree[A] = {
    var k: BKTree[A] = this
    for (v <- t.values)
      k = k + v
    k
  }

  def values: List[A] =
    this match {
      case BKTreeEmpty() => Nil
      case BKTreeNode(v, _, c) => v :: c.valuesIterator.toList.flatMap(_.values)
    }

  @tailrec
  final def -?-(a: A)(implicit m: MetricSpace[A]): Boolean =
    this match {
      case BKTreeEmpty() => false
      case BKTreeNode(v, _, c) => {
        val d = m.distance(v)(a)
        d == 0 || (c get d match {
          case None => false
          case Some(w) => w -?- a
        })
      }
    }

  def =?=(a: A, n: Int)(implicit m: MetricSpace[A]): Boolean =
    this match {
      case BKTreeEmpty() => false
      case BKTreeNode(v, _, c) => {
        val d = m.distance(v)(a)
        d <= n || (subChildren(d, n) exists (_._2 =?= (a, n)))
      }
    }

  def |=|(a: A, n: Int)(implicit m: MetricSpace[A]): List[A] =
    this match {
      case BKTreeEmpty() => Nil
      case BKTreeNode(v, _, c) => {
        val d = m.distance(v)(a)
        val k = subChildren(d, n).valuesIterator.toList flatMap (_ |=| (a, n))
        if (d <= n)
          v :: k
        else
          k
      }
    }

  private def subChildren(d: Int, n: Int): Map[Int, BKTree[A]] =
    this match {
      case BKTreeEmpty() => IntMap.empty
      case BKTreeNode(_, _, c) => subMap(c, d, n)
    }

  private def subMap(m: Map[Int, BKTree[A]], d: Int, n: Int): Map[Int, BKTree[A]] =
    splitMap(splitMap(m, d - n - 1)._2, d + n + 1)._1

  private def splitChildren(k: Int): (Map[Int, BKTree[A]], Map[Int, BKTree[A]]) =
    this match {
      case BKTreeEmpty() => (IntMap.empty, IntMap.empty)
      case BKTreeNode(_, _, c) => splitMap(c, k)
    }

  private def splitMap(m: Map[Int, BKTree[A]], k: Int): (Map[Int, BKTree[A]], Map[Int, BKTree[A]]) = {
    var m1: Map[Int, BKTree[A]] = IntMap.empty
    var m2: Map[Int, BKTree[A]] = IntMap.empty
    for ((i, v) <- m.iterator) {
      if (i < k)
        m1 = m1 + ((i, v))
      else if (i > k)
        m2 = m2 + ((i, v))
    }
    (m1, m2)
  }
}

private case class BKTreeNode[A](value: A, sz: Int, children: Map[Int, BKTree[A]]) extends BKTree[A]

private case class BKTreeEmpty[A]() extends BKTree[A]

object BKTree extends BKTrees {
  def apply[A]: BKTree[A] =
    emptyBKTree
}

trait BKTrees {
  def emptyBKTree[A]: BKTree[A] = BKTreeEmpty()
}
