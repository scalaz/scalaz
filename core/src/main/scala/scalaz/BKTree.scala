package scalaz

import collection.immutable.IntMap
import annotation.tailrec


/**
 * Burkhard-Keller trees provide an implementation of sets which apart
 * from the ordinary operations also has an approximate member search,
 * allowing you to search for elements that are of a distance `n` from
 * the element you are searching for. The distance is determined using
 * a metric on the type of elements. Therefore all elements must
 * implement the [[scalaz.MetricSpace]] type class, rather than the more usual
 * [[scalaz.Ordering]].
 *
 * The worst case complexity of many of these operations is quite bad,
 * but the expected behavior varies greatly with the metric. For
 * example, the discrete metric (`distance x y | y == x = 0 |
 * otherwise = 1`) makes BK-trees behave abysmally. The metrics
 * mentioned above should give good performance characteristics.
 *
 * This implementation is a port of Haskell's [[http://hackage.haskell.org/packages/archive/bktrees/0.2.1/doc/html/src/Data-Set-BKTree.html Data.Set.BKTree]]
 */
@deprecated("This class depends on `MetricSpace` which is deprecated, too.", "7.0.1")
sealed abstract class BKTree[A] extends Product with Serializable {
  def isEmpty: Boolean =
    this match {
      case BKTreeEmpty()       => true
      case BKTreeNode(_, _, _) => false
    }

  def map[B](f: A => B): BKTree[B] =
    this match {
      case BKTreeEmpty()       => BKTreeEmpty()
      case BKTreeNode(a, s, c) => BKTreeNode(f(a), s, c.transform((_: Int, z: BKTree[A]) => z map f))
    }

  def size: Int =
    this match {
      case BKTreeEmpty()       => 0
      case BKTreeNode(_, s, _) => s
    }

  def +(a: A)(implicit A: MetricSpace[A]): BKTree[A] =
    this match {
      case BKTreeEmpty()       => BKTreeNode(a, 1, IntMap.empty)
      case BKTreeNode(v, s, c) => {
        val d = A.distance(v, a)
        BKTreeNode(v, s + 1, c + ((d, c get d match {
          case None    => BKTreeNode(a, 1, IntMap.empty)
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

  def -(a: A)(implicit A: MetricSpace[A]): BKTree[A] =
    this match {
      case BKTreeEmpty()       => BKTreeEmpty()
      case BKTreeNode(v, _, c) => {
        val d = A.distance(v, a)
        if(d == 0) BKTree(c.values.seq.flatMap(_.values).toSeq: _*)
        else {
          val subTree = updateMap(c, d, (t: BKTree[A]) => Some(t - a))
          val size = subTree.values.map(_.size).sum + 1
          BKTreeNode(v, size, subTree)
        }
      }
    }

  def --(t: BKTree[A])(implicit m: MetricSpace[A]): BKTree[A] = {
    var k: BKTree[A] = this
    for (v <- t.values)
      k = k - v
    k
  }  

  def values: List[A] =
    this match {
      case BKTreeEmpty()       => Nil
      case BKTreeNode(v, _, c) => v :: c.valuesIterator.toList.flatMap(_.values)
    }


  /**
   * Returns true of this set contains `a`.
   */
  @tailrec
  final def contains(a: A)(implicit A: MetricSpace[A]): Boolean =
    this match {
      case BKTreeEmpty()       => false
      case BKTreeNode(v, _, c) =>
        val d = A.distance(v, a)
        d == 0 || (c get d match {
          case None    => false
          case Some(w) => w contains a
        })
    }

  /** An alias for `contains` */
  final def -?-(a: A)(implicit A: MetricSpace[A]): Boolean = contains(a)

  /** Returns true if this set contains an element which has a distance from `a` that is less than or equal to `n` */
  def containsApproximate(a: A, n: Int)(implicit A: MetricSpace[A]): Boolean =
    this match {
      case BKTreeEmpty()       => false
      case BKTreeNode(v, _, c) =>
        val d = A.distance(v, a)
        d <= n || (subChildren(d, n) exists (_._2 containsApproximate(a, n)))
    }

  /** An alias for `containsApproximate` */
  def =?=(a: A, n: Int)(implicit A: MetricSpace[A]): Boolean = containsApproximate(a, n)

  /** Returns the elements which have an distance from `a` that is less than or equal to `n`. */
  def valuesApproximate(a: A, n: Int)(implicit A: MetricSpace[A]): List[A] =
    this match {
      case BKTreeEmpty()       => Nil
      case BKTreeNode(v, _, c) =>
        val d = A.distance(v, a)
        val k = subChildren(d, n).valuesIterator.toList flatMap (_ valuesApproximate(a, n))
        if (d <= n)
          v :: k
        else
          k
    }

  /** An alias for `valuesApproximate` */
  def |=|(a: A, n: Int)(implicit A: MetricSpace[A]): List[A] = valuesApproximate(a, n)

  private type M[A] = IntMap[A]

  private def subChildren(d: Int, n: Int): M[BKTree[A]] =
    this match {
      case BKTreeEmpty()       => IntMap.empty
      case BKTreeNode(_, _, c) => subMap(c, d, n)
    }

  private def subMap(m: M[BKTree[A]], d: Int, n: Int): M[BKTree[A]] =
    splitMap(splitMap(m, d - n - 1)._2, d + n + 1)._1

  private def splitChildren(k: Int): (M[BKTree[A]], M[BKTree[A]]) =
    this match {
      case BKTreeEmpty()       => (IntMap.empty, IntMap.empty)
      case BKTreeNode(_, _, c) => splitMap(c, k)
    }

  private def splitMap(m: M[BKTree[A]], k: Int): (M[BKTree[A]], M[BKTree[A]]) = {
    var m1: M[BKTree[A]] = IntMap.empty
    var m2: M[BKTree[A]] = IntMap.empty
    for ((i, v) <- m.iterator) {
      if (i < k)
        m1 = m1 + ((i, v))
      else if (i > k)
        m2 = m2 + ((i, v))
    }
    (m1, m2)
  }

  private def updateMap(m: M[BKTree[A]], k: Int, f: BKTree[A] => Option[BKTree[A]]) =
    m get k match {
      case None => m
      case Some(v) => f(v) match {
        case None => m - k
        case Some(value) => m.updated(k, value)
      }
    }
}

private case class BKTreeNode[A](value: A, sz: Int, children: IntMap[BKTree[A]]) extends BKTree[A]

private case class BKTreeEmpty[A]() extends BKTree[A]

object BKTree extends BKTreeFunctions with BKTreeInstances {
  def apply[A: MetricSpace](as: A*): BKTree[A] = as.foldLeft(emptyBKTree[A])((b, a) => b + a)
}

trait BKTreeFunctions {
  def emptyBKTree[A]: BKTree[A] = BKTreeEmpty()
}

trait BKTreeInstances {
  implicit def bKTreeInstance: Functor[BKTree] with Length[BKTree] = new Functor[BKTree] with Length[BKTree] {
    def map[A, B](fa: BKTree[A])(f: A => B): BKTree[B] = fa map f
    def length[A](fa: BKTree[A]): Int = fa.size
  }
  implicit def bKTreeMonoid[A: MetricSpace]: Monoid[BKTree[A]] = new Monoid[BKTree[A]] {
    def append(f1: BKTree[A], f2: => BKTree[A]): BKTree[A] = f1 ++ f2
    def zero: BKTree[A] = BKTree[A]()
  }
  implicit def bkTreeEqual[A](implicit A: Equal[A]) = {
    import std.list._
    Equal.equalBy((ba: BKTree[A]) => ba.values)
  }
}
