package scalaz

// http://www.haskell.org/ghc/docs/7.0.2/html/libraries/containers-0.4.0.0/src/Data-Map.html

import Ordering.{ EQ, LT, GT }

import std.anyVal._
import std.option._

import annotation.tailrec

/** An immutable map of key/value pairs implemented as a balanced binary tree
 *
 * Based on Haskell's Data.Map
 *
 * @since 7.0.3 */
sealed abstract class ==>>[A, B] {
  import ==>>._

  /** number of key/value pairs - O(1) */
  val size: Int

  /** returns `true` if this map contains no key/value pairs - O(1) */
  def isEmpty: Boolean = this == empty

  /** tupled form of [[insert]] */
  def + (a: (A, B))(implicit o: Order[A]): A ==>> B =
    insert(a._1, a._2)

  /** inserts a new key/value - O(log n).
   *
   * If the key is already present, its value is replaced by the provided value.  */
  def insert(kx: A, x: B)(implicit n: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        singleton(kx, x)
      case Bin(ky, y, l, r) =>
        n.order(kx, ky) match {
          case LT =>
            balanceL(ky, y, l.insert(kx, x), r)
          case GT =>
            balanceR(ky, y, l, r.insert(kx, x))
          case EQ =>
            Bin(kx, x, l, r)
        }
    }

  /** inserts a new key/value pair, resolving the conflict if the key already exists - O(log n)
   *
   * @param f function to resolve conflict with existing key:
   *   (insertedValue, existingValue) => resolvedValue
   * @param kx key
   * @param x value to insert if the key is not already present */
  def insertWith(f: (B, B) => B, kx: A, x: B)(implicit o: Order[A]): A ==>> B =
    insertWithKey((_, a, b) => f(a,b), kx, x)

  /** inserts a new key/value pair, resolving the conflict if the key already exists - O(log n)
   *
   * @param f function to resolve conflict with existing key:
   *   (key, insertedValue, existingValue) => resolvedValue
   * @param kx key
   * @param x value to insert if the key is not already present */
  def insertWithKey(f: (A, B, B) => B, kx: A, x: B)(implicit o: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        singleton(kx, x)
      case Bin(ky, y, l, r) =>
        o.order(kx, ky) match {
          case LT =>
            balanceL(ky, y, l.insertWithKey(f, kx, x), r)
          case GT =>
            balanceR(ky, y, l, r.insertWithKey(f, kx, x))
          case EQ =>
            Bin(kx, f(kx, x, y), l, r)
        }
    }

  /** alias for [[delete]] */
  def -(k: A)(implicit o: Order[A]): A ==>> B =
    delete(k)

  /** removes a key/value pair - O(log n) */
  def delete(k: A)(implicit n: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        empty
      case Bin(kx, x, l, r) =>
        n.order(k, kx) match {
          case LT =>
            balanceR(kx, x, l.delete(k), r)
          case EQ =>
            glue(l, r)
          case GT =>
            balanceL(kx, x, l, r.delete(k))
        }
    }

  /** if the key exists, transforms its value - O(log n) */
  def adjust(k: A, f: B => B)(implicit o: Order[A]): A ==>> B =
    adjustWithKey(k, (_, x) => f(x))

  /** like [[adjust]] but with the key available in the transformation - O(log n) */
  def adjustWithKey(k: A, f: (A, B) => B)(implicit o: Order[A]): A ==>> B =
    updateWithKey(k, (a, b) => some(f(a, b)))

  /** updates or removes a value - O(log n)
   *
   * if `f` returns `None`, then the key is removed from the map */
  def update(k: A, f: B => Option[B])(implicit o: Order[A]): A ==>> B =
    updateWithKey(k, (_, x) => f(x))

  /** like [[update]] but with the key available in the update function - O(log n) */
  def updateWithKey(k: A, f: (A, B) => Option[B])(implicit o: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        empty
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            balanceR(kx, x, l.updateWithKey(k, f), r)
          case GT =>
            balanceL(kx, x, l, r.updateWithKey(k, f))
          case EQ =>
            f(kx, x) match {
              case Some(v) =>
                Bin(kx, v, l, r)
              case None =>
                glue(l, r)
            }
        }
    }

  /** looks up a key and updates its value - O(log n)
   *
   * Similar to [[updateWithKey]] but also returns the value. If the value was updated, returns the
   * new value. If the value was deleted, returns the old value. */
  def updateLookupWithKey(k: A, f: (A, B) => Option[B])(implicit o: Order[A]): (Option[B], A ==>> B) =
    this match {
      case Tip() =>
        (none, empty)
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            val (found, ll) = l.updateLookupWithKey(k, f)
            (found, balanceR(kx, x, ll, r))
          case GT =>
            val (found, rr) = r.updateLookupWithKey(k, f)
            (found, balanceL(kx, x, l, rr))
          case EQ =>
            f(kx, x) match {
              case Some(xx) =>
                (some(xx), Bin(kx, xx, l, r))
              case None =>
                (some(x), glue(l, r))
            }
        }
    }

  def alter(k: A, f: Option[B] => Option[B])(implicit o: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        f(None) match {
          case None =>
            empty
          case Some(x) =>
            singleton(k, x)
        }

      case Bin(kx, x, l,r) =>
        o.order(k, kx) match {
          case LT =>
            balance(kx, x, l.alter(k, f), r)
          case GT =>
            balance(kx, x, l, r.alter(k, f))
          case EQ =>
            f(some(x)) match {
              case None =>
                glue(l, r)
              case Some(xx) =>
                Bin(kx, xx, l, r)
            }
        }
    }

  @tailrec
  final def lookup(k: A)(implicit n: Order[A]): Option[B] =
    this match {
      case Tip() =>
        none
      case Bin(kx, x, l, r) =>
        n.order(k, kx) match {
          case LT =>
            l.lookup(k)
          case GT =>
            r.lookup(k)
          case EQ =>
            some(x)
        }
    }

  @tailrec
  final def lookupAssoc(k: A)(implicit n: Order[A]): Option[(A, B)] =
    this match {
      case Tip() =>
        none
      case Bin(kx, x, l, r) =>
        n.order(k, kx) match {
          case LT =>
            l.lookupAssoc(k)
          case GT =>
            r.lookupAssoc(k)
          case EQ =>
            some((kx, x))
        }
    }

  def values: List[B] =
    foldrWithKey(List.empty[B])((_, x, xs) => x :: xs)

  def keys: List[A] =
    foldrWithKey(List.empty[A])((x, _, xs) => x :: xs)

  def keySet: ISet[A] = this match {
    case Tip()        => ISet.Tip[A]
    case Bin(k,v,l,r) => ISet.Bin(k,l.keySet,r.keySet)
  }

  def toList: List[(A, B)] =
    toAscList

  def toAscList: List[(A, B)] =
    foldrWithKey(List.empty[(A, B)])((k, x, xs) => (k, x) :: xs)

  def toDescList: List[(A, B)] =
    foldlWithKey(List.empty[(A, B)])((xs, k, x) => (k, x) :: xs)

  def member(k: A)(implicit n: Order[A]): Boolean =
    lookup(k)(n).isDefined

  def notMember(k: A)(implicit n: Order[A]): Boolean =
    !member(k)

  def lookupIndex(k: A)(implicit o: Order[A]): Option[Int] = {
    @tailrec
    def go(n: Int, m: A ==>> B): Option[Int] =
      m match {
        case Tip() =>
          none
        case Bin(kx, x, l, r) =>
          o.order(k, kx) match {
            case LT =>
              go(n, l)
            case GT =>
              go(n + l.size + 1, r)
            case EQ =>
              some((n + l.size))
          }
      }

    go(0, this)
  }

  @tailrec
  final def elemAt(i: Int): Option[(A, B)] =
    this match {
      case Tip() =>
        none
      case Bin(kx, x, l ,r) =>
        implicitly[Order[Int]].order(i, l.size) match {
          case LT =>
            l.elemAt(i)
          case GT =>
            r.elemAt(i - l.size - 1)
          case EQ =>
            some((kx, x))
        }
    }

  // TODO: This should be a total function
  def updateAt(i: Int, f: (A, B) => Option[B]): A ==>> B =
    this match {
      case Tip() =>
        sys.error("updateAt")
      case Bin(kx, x, l, r) =>
        implicitly[Order[Int]].order(i, l.size) match {
          case LT =>
            balanceR(kx, x, l.updateAt(i, f), r)
          case GT =>
            balanceL(kx, x, l, r.updateAt(i - l.size - 1, f))
          case EQ =>
            f(kx, x) match {
              case Some(y) => Bin(kx, y, l, r)
              case None => glue(l, r)
            }
        }
    }

  def deleteAt(i: Int): A ==>> B =
    updateAt(i, (A, B) => None)

  @tailrec
  final def findMin: Option[(A, B)] =
    this match {
      case Bin(kx, x, Tip(), _) =>
        some((kx, x))
      case Bin(_, _, l, _) =>
        l.findMin
      case Tip() =>
        none
    }

  @tailrec
  final def findMax: Option[(A, B)] =
    this match {
      case Bin(kx, x, _, Tip()) =>
        some((kx, x))
      case Bin(_, _, _, r) =>
        r.findMax
      case Tip() =>
        none
    }

  def deleteMin: A ==>> B =
    this match {
      case Bin(_, _, Tip(), r) =>
        r
      case Bin(kx, x, l, r) =>
        balanceR(kx, x, l.deleteMin, r)
      case Tip() =>
        empty
    }

  def deleteMax: A ==>> B =
    this match {
      case Bin(_, _, l, Tip()) =>
        l
      case Bin(kx, x, l, r) =>
        balanceL(kx, x, l, r.deleteMax)
      case Tip() =>
        empty
    }

  def updateMin(f: B => Option[B]): A ==>> B =
    updateMinWithKey((_: A, b) => f(b))

  def updateMinWithKey(f: (A, B) => Option[B]): A ==>> B =
    this match {
      case Bin(kx, x, Tip(), r) =>
        f(kx, x) match {
          case None =>
            r
          case Some(s) =>
            Bin(kx, s, Tip(), r)
        }
      case Bin(kx, x, l, r) =>
        balanceR(kx, x, l.updateMinWithKey(f), r)
      case Tip() =>
        empty
    }

  def updateMax(f: B => Option[B]): A ==>> B =
    updateMaxWithKey((_: A, b) => f(b))

  def updateMaxWithKey(f: (A, B) => Option[B]): A ==>> B =
    this match {
      case Bin(kx, x, l, Tip()) =>
        f(kx, x) match {
          case None =>
            l
          case Some(s) =>
            Bin(kx, s, l, Tip())
        }
      case Bin(kx, x, l, r) =>
        balanceL(kx, x, l, r.updateMaxWithKey(f))
      case Tip() =>
        empty
    }

  /**
    * insert v into the map at k. If there is already a value for k,
    * append to the existing value using the Semigroup
    */
  def updateAppend(k: A, v: B)(implicit o: Order[A], bsg: Semigroup[B]): A ==>> B =
    alter(k, old ⇒ Some(old.map(bsg.append(_, v)).getOrElse(v)))

  def minViewWithKey: Option[((A, B), A ==>> B)] =
    this match {
      case Tip() =>
        none
      case x @ Bin(_, _, _, _) =>
        some(deleteFindMin(x))
    }

  def maxViewWithKey: Option[((A, B), A ==>> B)] =
    this match {
      case Tip() =>
        none
      case x @ Bin(_, _, _, _) =>
        some(deleteFindMax(x))
    }

  def minView: Option[(B, A ==>> B)] =
    this match {
      case Tip() =>
        none
      case x @ Bin(_, _, _, _) =>
        val r = deleteFindMin(x)
        some((r._1._2, r._2))
    }

  def maxView: Option[(B, A ==>> B)] =
    this match {
      case Tip() =>
        none
      case x @ Bin(_, _, _, _) =>
        val r = deleteFindMax(x)
        some((r._1._2, r._2))
    }

  def deleteFindMax(t: Bin[A, B]): ((A, B), A ==>> B) =
    t match {
      case Bin(k, x, l, Tip()) =>
        ((k,x), l)
      case Bin(k, x, l, r @ Bin(_, _, _, _)) =>
        val (km, r2) = deleteFindMax(r)
        (km, balanceL(k, x, l, r2))
    }

  def deleteFindMin(t: Bin[A, B]): ((A, B), A ==>> B) =
    t match {
      case Bin(k, x, Tip(), r) =>
        ((k, x), r)
      case Bin(k, x, l @ Bin(_, _, _, _), r) =>
        val (km, l2) = deleteFindMin(l)
        (km, balanceR(k, x, l2, r))
    }

  /* Mappings */
  def map[C](f: B => C): A ==>> C =
    mapWithKey((_, x: B) => f(x))

  def mapWithKey[C](f: (A, B) => C): A ==>> C =
    this match {
      case Tip() =>
        empty
      case Bin(kx, x, l, r) =>
        Bin(kx, f(kx, x), l.mapWithKey(f), r.mapWithKey(f))
    }

  def mapAccum[C](z: C)(f: (C, B) => (C, B)): (C, A ==>> B) =
    mapAccumWithKey(z)((a2, _, x2) => f(a2, x2))

  def mapAccumWithKey[C](z: C)(f: (C, A, B) => (C, B)): (C, A ==>> B) =
    mapAccumL(z)(f)

  def mapAccumL[C](a: C)(f: (C, A, B) => (C, B)): (C, A ==>> B) =
    this match {
      case Tip() =>
        (a, empty)
      case Bin(kx, x, l, r) =>
        val (a1, l2) = l.mapAccumL(a)(f)
        val (a2, x2) = f(a1, kx, x)
        val (a3, r2) = r.mapAccumL(a2)(f)
        (a3, Bin(kx, x2, l2, r2))
    }

  //def mapAccumRWithKey

  def mapKeys[C](f: A => C)(implicit o: Order[C]): C ==>> B =
    foldlWithKey(empty[C, B])((xs, k, x) => xs.insert(f(k), x))

  def mapKeysWith[C](f: A => C, f2: (B, B) => B)(implicit o: Order[C]): C ==>> B =
    fromListWith[C, B](toList.map(x => (f(x._1), x._2)))(f2)

  /* Folds */
  def fold[C](z: C)(f: (A, B, C) => C): C =
    foldrWithKey(z)(f)

  def foldlWithKey[C](z: C)(f:  (C, A, B) => C): C =
    this match {
      case Tip() =>
        z
      case Bin(kx, x, l, r) =>
        r.foldlWithKey(f(l.foldlWithKey(z)(f), kx, x))(f)
    }

  def foldrWithKey[C](z: C)(f: (A, B, C) => C): C =
    this match {
      case Tip() =>
        z
      case Bin(kx, x, l, r) =>
        l.foldrWithKey(f(kx, x, r.foldrWithKey(z)(f)))(f)
    }

  /* Unions */
  def union(other: A ==>> B)(implicit k: Order[A]): A ==>> B = {
    def hedgeUnion(blo: Option[A], bhi: Option[A], m1: A ==>> B, m2: A ==>> B): A ==>> B =
      (m1, m2) match {
        case (t1, Tip()) =>
          t1
        case (Tip(), Bin(kx, x, l, r)) =>
          link(kx, x, l filterGt blo, r filterLt bhi)
        case (t1, Bin(kx, x, Tip(), Tip())) =>
          insertR(kx, x, t1)
        case (Bin(kx, x, l, r), t2) =>
          val bmi = Some(kx)
          val nm1 = hedgeUnion(blo, bmi, l, ==>>.trim(blo, bmi, t2))
          val nm2 = hedgeUnion(bmi, bhi, r, ==>>.trim(bmi, bhi, t2))
          link(kx, x, nm1, nm2)
      }

    def insertR(kx: A, x: B, t: A ==>> B)(implicit o: Order[A]): A ==>> B = {
      def go(kx: A, x: B, m: A ==>> B): A ==>> B =
        m match {
          case Tip() => singleton(kx, x)
          case Bin(ky, y, l, r) =>
            o.order(kx, ky) match {
              case LT => balanceL(ky, y, go(kx, x, l), r)
              case GT => balanceR(ky, y, l, go(kx, x, r))
              case EQ => m
            }
        }
      go(kx, x, t)
    }

    (this, other) match {
      case (Tip(), t2) => t2
      case (t1, Tip()) => t1
      case (t1, t2)    => hedgeUnion(None, None, t1, t2)
    }
  }

  def unionWith(other: A ==>> B)(f: (B, B) => B)(implicit o: Order[A]): A ==>> B =
    unionWithKey(other)((_: A, b: B, c: B) => f(b, c))

  def unionWithKey(other: A ==>> B)(f: (A, B, B) => B)(implicit o: Order[A]): A ==>> B = {
    def hedgeUnionWithKey(cmplo: A => Ordering, cmphi: A => Ordering, a: A ==>> B, b: A ==>> B): A ==>> B =
      (a, b) match {
        case (t1, Tip()) =>
          t1
        case (Tip(), Bin(kx, x, l, r)) =>
          link(kx, x, l filterGt cmplo, r filterLt cmphi)
        case (Bin(kx, x, l, r), t2) =>
          val cmpkx = (k: A) => o.order(kx, k)
          val (found, gt) = t2.trimLookupLo(kx, cmphi)
          val newx = found match {
            case None => x
            case Some((_, y)) => f(kx, x, y)
          }
          val aa = hedgeUnionWithKey(cmplo, cmpkx, l, t2.trim(cmplo, cmpkx))
          val bb = hedgeUnionWithKey(cmpkx, cmphi, r, gt)
          link(kx, newx, aa, bb)
      }

    (this, other) match {
      case (Tip(), t2) =>
        t2
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeUnionWithKey(Function const LT, Function const GT, t1, t2)
    }
  }

  // Difference functions
  def \\[C](other: A ==>> C)(implicit o: Order[A]): A ==>> B =
    difference(other)

  def difference[C](other: A ==>> C)(implicit o: Order[A]): A ==>> B = {
    def hedgeDiff(blo: Option[A], bhi: Option[A], a: A ==>> B, b: A ==>> C): A ==>> B =
      (a, b) match {
        case (Tip(), _) =>
          empty
        case (Bin(kx, x, l, r), Tip()) =>
          link(kx, x, l filterGt blo, r filterLt bhi)
        case (t, Bin(kx, _, l, r)) =>
          val bmi = some(kx)
          val aa = hedgeDiff(blo, bmi, ==>>.trim(blo, bmi, t), l)
          val bb = hedgeDiff(bmi, bhi, ==>>.trim(bmi, bhi, t), r)
          aa merge bb
      }

    (this, other) match {
      case (Tip(), _) =>
        empty
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeDiff(None, None, t1, t2)
    }
  }

  def differenceWith[C](other: A ==>> C, f: (B, C) => Option[B])(implicit o: Order[A]): A ==>> B =
    differenceWithKey(other, (_: A, b: B, c: C) => f(b, c))

  def differenceWithKey[C](other: A ==>> C, f: (A, B, C) => Option[B])(implicit o: Order[A]): A ==>> B = {
    def hedgeDiffWithKey(cmplo: A => Ordering, cmphi: A => Ordering, a: A ==>> B, b: A ==>> C): A ==>> B =
      (a, b) match {
        case (Tip(), _) =>
          empty
        case (Bin(kx, x, l, r), Tip()) =>
          link(kx, x, l filterGt cmplo, r filterLt cmphi)
        case (t, Bin(kx, x, l, r)) =>
          val cmpkx = (k: A) => o.order(kx, k)
          val (found, gt) = t.trimLookupLo(kx, cmphi)
          val lt = t.trim(cmplo, cmpkx)
          val tl = hedgeDiffWithKey(cmplo, cmpkx, lt, l)
          val tr = hedgeDiffWithKey(cmpkx, cmphi, gt, r)

          found match {
            case None =>
              tl merge tr
            case Some((ky, y)) =>
              f(ky, y, x) match {
                case None =>
                  tl merge tr
                case Some(z) =>
                  link(kx, z, tl, tr)
              }
          }
      }

    (this, other) match {
      case (Tip(), _) =>
        empty
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeDiffWithKey(Function const LT, Function const GT, t1, t2)
    }
  }

  // Intersections
  def intersection[C](other: A ==>> C)(implicit o: Order[A]): A ==>> B =
    intersectionWithKey(other)((_, x, _: C) => x)

  def intersectionWith[C, D](other: A ==>> C)(f: (B, C) => D)(implicit o: Order[A]): A ==>> D =
    intersectionWithKey(other)((_, x, y: C) => f(x, y))

  def intersectionWithKey[C, D](other: A ==>> C)(f: (A, B, C) => D)(implicit o: Order[A]): A ==>> D =
    (this, other) match {
      case (Tip(), _) =>
        empty
      case (_, Tip()) =>
        empty
      case (t1 @ Bin(k1, x1, l1, r1), t2 @ Bin(k2, x2, l2, r2)) =>
        if (t1.size >= t2.size) {
          val (lt, found, gt) = t1 splitLookupWithKey k2
          val tl = lt.intersectionWithKey(l2)(f)
          val tr = gt.intersectionWithKey(r2)(f)
          found match {
            case None =>
              tl merge tr
            case Some((k, x)) =>
              link(k, f(k, x, x2), tl, tr)
          }
        }
        else {
          val (lt, found, gt) = t2 splitLookup k1
          val tl = l1.intersectionWithKey(lt)(f)
          val tr = r1.intersectionWithKey(gt)(f)
          found match {
            case None =>
              tl merge tr
            case Some(x) =>
              link(k1, f(k1, x1, x), tl, tr)
          }
        }
    }

  // Submap
  def isSubmapOf(a: A ==>> B)(implicit o: Order[A], e: Equal[B]): Boolean =
    isSubmapOfBy(a, e.equal)

  def isSubmapOfBy(a: A ==>> B, f: (B, B) => Boolean)(implicit o: Order[A]): Boolean =
    size <= a.size && submap(a, f)

  private[scalaz] def submap(a: A ==>> B, f: (B, B) => Boolean)(implicit o: Order[A]): Boolean =
    (this, a) match {
      case (Tip(), _) =>
        true
      case (_, Tip()) =>
        false
      case (Bin(kx, x, l, r), t) =>
        val (lt, found, gt) = t splitLookup kx
        found match {
          case None =>
            false
          case Some(y) =>
            f(x, y) && l.submap(lt, f) && r.submap(gt, f)
        }
    }

  // Filter
  def filter(p: B => Boolean)(implicit o: Order[A]): A ==>> B =
    filterWithKey((_, x) => p(x))

  def filterWithKey(p: (A, B) => Boolean)(implicit o: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        empty
      case Bin(kx, x, l, r) =>
        if (p(kx, x))
          link(kx, x, l.filterWithKey(p), r.filterWithKey(p))
        else
          l.filterWithKey(p) merge r.filterWithKey(p)
    }

  // Partition
  def partition(p: B => Boolean)(implicit o: Order[A]): (A ==>> B, A ==>> B) =
    partitionWithKey((_, x) => p(x))

  def partitionWithKey(p: (A, B) => Boolean)(implicit o: Order[A]): (A ==>> B, A ==>> B) =
    this match {
      case Tip() =>
        (empty, empty)
      case Bin(kx, x, l, r) =>
        val (l1, l2) = l partitionWithKey p
        val (r1, r2) = r partitionWithKey p

        if (p(kx, x))
          (link(kx, x, l1, r1), l2 merge r2)
        else
          (l1 merge r1, link(kx, x, l2, r2))
    }

  def mapOption[C](f: B => Option[C])(implicit o: Order[A]): A ==>> C =
    mapOptionWithKey((_, x) => f(x))

  def mapOptionWithKey[C](f: (A, B) => Option[C])(implicit o: Order[A]): A ==>> C =
    this match {
      case Tip() =>
        empty
      case Bin(kx, x, l, r) =>
        f(kx, x) match {
          case Some(y) =>
            link(kx, y, l.mapOptionWithKey(f), r.mapOptionWithKey(f))
          case None =>
            l.mapOptionWithKey(f).merge(r.mapOptionWithKey(f))
        }
    }

  def mapEither[C, D](f: B => C \/ D)(implicit o: Order[A]): (A ==>> C, A ==>> D) =
    mapEitherWithKey((_, x) => f(x))

  def mapEitherWithKey[C, D](f: (A, B) => C \/ D)(implicit o: Order[A]): (A ==>> C, A ==>> D) =
    this match {
      case Tip() =>
        (empty, empty)
      case Bin(kx, x, l, r) =>
        val (l1, l2) = l.mapEitherWithKey(f)
        val (r1, r2) = r.mapEitherWithKey(f)

        f(kx, x) match {
          case -\/(y) =>
            (link(kx, y, l1, r1), l2 merge r2)
          case \/-(z) =>
            (l1 merge r1, link(kx, z, l2, r2))
        }
    }

  // Split
  def split(k: A)(implicit o: Order[A]): (A ==>> B, A ==>> B) =
    this match {
      case Tip() =>
        (empty, empty)
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            val (lt, gt) = l.split(k)
            (lt, link(kx, x, gt, r))
          case GT =>
            val (lt, gt) = r.split(k)
            (link(kx, x, l, lt), gt)
          case EQ =>
            (l, r)
        }
    }

  def splitLookup(k: A)(implicit o: Order[A]): (A ==>> B, Option[B], A ==>> B) =
    this match {
      case Tip() =>
        (empty, none, empty)
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            val (lt, z, gt) = l splitLookup k
            (lt, z, link(kx, x, gt, r))
          case GT =>
            val (lt, z, gt) = r splitLookup k
            (link(kx, x, l, lt), z, gt)
          case EQ =>
            (l, some(x), r)
        }
    }

  def splitLookupWithKey(k: A)(implicit o: Order[A]): (A ==>> B, Option[(A, B)], A ==>> B) =
    this match {
      case Tip() =>
        (empty, none, empty)
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            val (lt, z, gt) = l splitLookupWithKey k
            (lt, z, link(kx, x, gt, r))
          case GT =>
            val (lt, z, gt) = r splitLookupWithKey k
            (link(kx, x, l, lt), z, gt)
          case EQ =>
            (l, some((kx, x)), r)
        }
    }

  // Utility functions
  // Because the following functions depend on some public interface, such as deleteFindMax,
  // they can't be moved to object ==>> unlike other utility functions, balance(...) for example.

  // TODO: merge should be private as it's a utility function but not a public interface.
  protected def merge(other: A ==>> B): A ==>> B =
    (this, other) match {
      case (Tip(), r) =>
        r
      case (l, Tip()) =>
        l
      case (l @ Bin(kx, x, lx, rx), r @ Bin(ky, y, ly, ry)) =>
        if (delta * l.size < r.size) balanceL(ky, y, l.merge(ly), ry)
        else if (delta * r.size < l.size) balanceR(kx, x, lx, rx.merge(r))
        else glue(l, r)
    }

  private def glue(l: A ==>> B, r: A ==>> B): A ==>> B =
    (l, r) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (l @ Bin(_, _, _, _), r @ Bin(_, _, _, _)) => if (l.size > r.size) {
        val ((km, m), l2) = deleteFindMax(l)
        balanceR(km, m, l2, r)
      }
      else {
        val ((km, m), r2) = deleteFindMin(r)
        balanceL(km, m, l, r2)
      }
    }

  @deprecated("trim is no longer a public function", "7.3")
  @tailrec
  final def trim(lo: A => Ordering, hi: A => Ordering): A ==>> B =
    this match {
      case Tip() =>
        empty
      case t @ Bin(kx, _, l, r) =>
        lo(kx) match {
          case LT => hi(kx) match {
            case GT => t
            case _ => l.trim(lo, hi)
          }
          case _ => r.trim(lo, hi)
        }
    }

  @deprecated("trimLookupLo is no longer a public function", "7.3")
  @tailrec
  final def trimLookupLo(lo: A, cmphi: A => Ordering)(implicit o: Order[A]): (Option[(A, B)], A ==>> B) =
    this match {
      case Tip() =>
        (none, empty)
      case t @ Bin(kx, x, l, r) =>
        o.order(lo, kx) match {
          case LT =>
            cmphi(kx) match {
              case GT =>
                (t lookupAssoc lo, t)
              case _ =>
                l.trimLookupLo(lo, cmphi)
          }
          case GT =>
            r.trimLookupLo(lo, cmphi)
          case EQ =>
            (some((kx, x)), r.trim(a => o.order(lo, a), cmphi))
        }
    }

  override final def equals(other: Any): Boolean =
    other match {
      case that: ==>>[A, B] =>
        ==>>.mapEqual[A, B](Equal.equalA, Equal.equalA).equal(this, that)
      case _ =>
        false
    }

  override final def hashCode: Int =
    toAscList.hashCode

  // filters on keys
  private def filterGt(a: Option[A])(implicit o: Order[A]): A ==>> B = {
    def filter(filteringKey: A, m: A ==>> B): A ==>> B =
      m match {
        case Tip() =>
          empty
        case Bin(kx, x, l, r) =>
          o.order(filteringKey, kx) match {
            case LT => link(kx, x, filter(filteringKey, l), r)
            case EQ => r
            case GT => filter(filteringKey, r)
          }
      }

    cata(a)(filter(_, this), this)
  }

  private def filterLt(a: Option[A])(implicit o: Order[A]): A ==>> B = {
    def filter(filteringKey: A, m: A ==>> B): A ==>> B =
      m match {
        case Tip() =>
          empty
        case Bin(kx, x, l, r) =>
          o.order(kx, filteringKey) match {
            case LT => link(kx, x, l, filter(filteringKey, r))
            case EQ => l
            case GT => filter(filteringKey, l)
          }
      }

    cata(a)(filter(_, this), this)
  }

  @deprecated("join is no longer a protected function", "7.3")
  protected def join(kx: A, x: B, other: A ==>> B)(implicit o: Order[A]): A ==>> B =
    (this, other) match {
      case (Tip(), r) =>
        insertMin(kx, x, r)
      case (l, Tip()) =>
        insertMax(kx, x, l)
      case (l @ Bin(ky, y, ly, ry), r @ Bin(kz, z, lz, rz)) =>
        if (delta * l.size < r.size) balanceL(kz, z, link(kx, x, l, lz), rz)
        else if (delta * r.size < l.size) balanceR(ky, y, ly, link(kx, x, ry, r))
        else Bin(kx, x, l, r)
    }
}

sealed abstract class MapInstances0 {

  implicit def scalazMapInstance[S: Order]: Bind[S ==>> ?] with Align[S ==>> ?] with Zip[S ==>> ?] =
    new Bind[S ==>> ?] with Align[S ==>> ?] with Zip[S ==>> ?] {
      override def map[A, B](fa: S ==>> A)(f: A => B) =
        fa map f

      def bind[A, B](fa: S ==>> A)(f: A => (S ==>> B)) =
        fa.mapOptionWithKey((k, v) => f(v).lookup(k))

      import \&/._, ==>>.Tip

      override def align[A, B](a: S ==>> A, b: S ==>> B) =
        (a, b) match {
          case (Tip(), Tip()) => Tip()
          case (a    , Tip()) => a.map(This(_))
          case (Tip(), b    ) => b.map(That(_))
          case (a    , b    ) =>
            a.map(This(_): A \&/ B).unionWith(b.map(That(_): A \&/ B)){
              case (This(aa), That(bb)) => Both(aa, bb)
              case _ => sys.error("==>> align")
            }
         }

      override def alignWith[A, B, C](f: A \&/ B => C) = {
        case (Tip(), Tip()) => Tip()
        case (a    , Tip()) => a.map(aa => f(This(aa)))
        case (Tip(), b    ) => b.map(bb => f(That(bb)))
        case (a    , b    ) =>
          a.map(This(_): A \&/ B).unionWith(b.map(That(_): A \&/ B)){
            case (This(aa), That(bb)) => Both(aa, bb)
            case _ => sys.error("==>> alignWith")
          }.map(f)
        }

      def zip[A, B](a: => (S ==>> A), b: => (S ==>> B)) = {
        val a0 = a
        if(a0.isEmpty) ==>>.empty
        else a0.intersectionWith(b)(Tuple2.apply)
      }

      override def zipWith[A, B, C](a: => (S ==>> A), b: => (S ==>> B))(f: (A, B) => C)(implicit F: Functor[S ==>> ?]) = {
        val a0 = a
        if(a0.isEmpty) ==>>.empty
        else a0.intersectionWith(b)(f)
      }
    }
}

sealed abstract class MapInstances extends MapInstances0 {
  import ==>>._

  import std.list._
  import std.tuple._

  implicit def mapShow[A: Show, B: Show]: Show[==>>[A, B]] =
    Contravariant[Show].contramap(Show[List[(A, B)]])(_.toAscList)

  implicit def mapEqual[A: Equal, B: Equal]: Equal[A ==>> B] =
    new MapEqual[A, B] {def A = implicitly; def B = implicitly}

  implicit def mapOrder[A: Order, B: Order]: Order[A ==>> B] =
    new Order[A ==>> B] with MapEqual[A, B] {
      def A = implicitly
      def B = implicitly
      def order(o1: A ==>> B, o2: A ==>> B) =
        Order[List[(A,B)]].order(o1.toAscList, o2.toAscList)
    }

  implicit def mapUnion[A, B](implicit A: Order[A], B: Semigroup[B]): Monoid[A ==>> B] =
    Monoid.instance((l, r) => (l unionWith r)(B.append(_, _)), Tip())

  implicit def mapIntersection[A, B](implicit A: Order[A], B: Semigroup[B]
                                   ): Semigroup[(A ==>> B) @@ Tags.Conjunction] =
    Tag.subst(Semigroup.instance((l, r) =>
      (l intersectionWith r)(B.append(_, _))))

  implicit def mapCovariant[S]: Traverse[S ==>> ?] =
    new Traverse[S ==>> ?] {
      override def findLeft[A](fa: S ==>> A)(f: A => Boolean): Option[A] =
        fa match {
          case Bin(_, x, l, r) =>
            findLeft(l)(f) match {
              case a @ Some(_) =>
                a
              case None =>
                if (f(x))
                  Some(x)
                else
                  findLeft(r)(f)
            }
          case Tip() =>
            None
        }

      override def findRight[A](fa: S ==>> A)(f: A => Boolean): Option[A] =
        fa match {
          case Bin(_, x, l, r) =>
            findRight(r)(f) match {
              case a @ Some(_) =>
                a
              case None =>
                if (f(x))
                  Some(x)
                else
                  findRight(l)(f)
            }
          case Tip() =>
            None
        }

      override def map[A, B](fa: S ==>> A)(f: A => B): S ==>> B =
        fa map f

      override def foldMap[A, B](fa: S ==>> A)(f: A => B)(implicit F: Monoid[B]): B =
        fa match {
          case Tip() =>
            F.zero
          case Bin(k, x, l, r) =>
            F.append(foldMap(l)(f), F.append(f(x), foldMap(r)(f)))
        }

      override def foldRight[A, B](fa: S ==>> A, z: => B)(f: (A, => B) => B): B =
        fa.foldrWithKey(z)((_, b, acc) => f(b, acc))

      override def foldLeft[A, B](fa: S ==>> A, z: B)(f: (B, A) => B): B =
        fa.foldlWithKey(z)((acc, _, b) => f(acc, b))

      override def index[A](fa: S ==>> A, i: Int): Option[A] =
        fa.elemAt(i).map(_._2)

      def traverseImpl[F[_], A, B](fa: S ==>> A)(f: A => F[B])(implicit G: Applicative[F]): F[S ==>> B] =
        fa match {
          case Tip() =>
            G.point(Tip())
          case Bin(kx, x, l, r) =>
            G.apply3(traverseImpl(l)(f), f(x), traverseImpl(r)(f)){
              (l2, x2, r2) => Bin(kx, x2, l2, r2)
            }
        }

      override def length[A](fa: S ==>> A): Int =
        fa.size

      override def any[A](fa: S ==>> A)(f: A => Boolean): Boolean =
        fa match {
          case Tip() => false
          case Bin(_, x, l, r) =>
            any(l)(f) || f(x) || any(r)(f)
        }

      override def all[A](fa: S ==>> A)(f: A => Boolean): Boolean =
        fa match {
          case Tip() => true
          case Bin(_, x, l, r) =>
            all(l)(f) && f(x) && all(r)(f)
        }

    }

  implicit val mapBifoldable: Bifoldable[==>>] = new Bifoldable[==>>] {
    def bifoldMap[A,B,M](fa: A ==>> B)(f: A => M)(g: B => M)(implicit F: Monoid[M]): M =
      fa match {
        case Tip() =>
          F.zero
        case Bin(k, x, l, r) =>
          F.append(bifoldMap(l)(f)(g),
                   F.append(f(k), F.append(g(x), bifoldMap(r)(f)(g))))
      }

    def bifoldRight[A,B,C](fa: A ==>> B, z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
      fa.foldrWithKey(z)((a, b, c) => f(a, g(b, c)))

    override def bifoldLeft[A,B,C](fa: A ==>> B, z: C)(f: (C, A) => C)(g: (C, B) => C): C =
      fa.foldlWithKey(z)((c, a, b) => g(f(c, a), b))
  }
}

private[scalaz] sealed trait MapEqual[A, B] extends Equal[A ==>> B] {
  import std.list._
  import std.tuple._

  implicit def A: Equal[A]
  implicit def B: Equal[B]
  final override def equal(a1: A ==>> B, a2: A ==>> B): Boolean =
    Equal[Int].equal(a1.size, a2.size) && Equal[List[(A, B)]].equal(a1.toAscList, a2.toAscList)
}

object ==>> extends MapInstances {
  private[scalaz] case object Tip extends (Nothing ==>> Nothing) {
    val size = 0

    def unapply[A, B](a: A ==>> B): Boolean = a eq this

    def apply[A, B](): A ==>> B = this.asInstanceOf[A ==>> B]
  }

  private[scalaz] final case class Bin[A, B](k: A, v: B, l: A ==>> B, r: A ==>> B) extends ==>>[A, B] {
    val size = l.size + r.size + 1
  }

  final def apply[A: Order, B](x: (A, B)*): A ==>> B =
    x.foldLeft(empty[A, B])((a, c) => a.insert(c._1, c._2))

  final def empty[A, B]: A ==>> B =
    Tip[A, B]()

  final def singleton[A, B](k: A, x: B): A ==>> B =
    Bin(k, x, Tip(), Tip())

  /* List operations */
  final def fromList[A: Order, B](l: List[(A, B)]): A ==>> B =
    l.foldLeft(empty[A, B]) { (t, x) => t.insert(x._1, x._2) }

  final def fromListWith[A: Order, B](l: List[(A, B)])(f: (B, B) => B): A ==>> B =
    fromListWithKey(l)((_, x, y) => f(x, y))

  final def fromListWithKey[A: Order, B](l: List[(A, B)])(f: (A, B, B) => B): A ==>> B =
    l.foldLeft(empty[A, B])((a, c) => a.insertWithKey(f, c._1, c._2))

  /* Foldable operations */
  final def fromFoldable[F[_]: Foldable, A: Order, B](fa: F[(A, B)]): A ==>> B =
    Foldable[F].foldLeft(fa, empty[A, B]) { (t, x) => t.insert(x._1, x._2) }

  final def fromFoldableWith[F[_]: Foldable, A: Order, B](fa: F[(A, B)])(f: (B, B) => B): A ==>> B =
    fromFoldableWithKey(fa)((_, x, y) => f(x, y))

  final def fromFoldableWithKey[F[_]: Foldable, A: Order, B](fa: F[(A, B)])(f: (A, B, B) => B): A ==>> B =
    Foldable[F].foldLeft(fa, empty[A, B])((a, c) => a.insertWithKey(f, c._1, c._2))

  final def unions[A: Order, B](xs: List[A ==>> B]): A ==>> B =
    xs.foldLeft(empty[A, B])((a, c) => a.union(c))

  final def unionsWith[A: Order, B](f: (B, B) => B)(xs: List[A ==>> B]): A ==>> B =
    xs.foldLeft(empty[A, B])((a, c) => a.unionWith(c)(f))

  private[scalaz] final val ratio = 2
  private[scalaz] final val delta = 3

  // Even though we have balanceL and balanceR, we need balance function
  // as alter function still needs it.
  private[scalaz] def balance[A, B](k: A, x: B, l: A ==>> B, r: A ==>> B): A ==>> B =
    l match {
      case Tip() =>
        r match {
          case Tip() =>
            singleton(k, x)
          case Bin(_, _, Tip(), Tip()) =>
            Bin(k, x, Tip(), r)
          case Bin(rk, rx, Tip(), rr@Bin(_, _, _, _)) =>
            Bin(rk, rx, singleton(k, x), rr)
          case Bin(rk, rx, Bin(rlk, rlx, _, _), Tip()) =>
            Bin(rlk, rlx, singleton(k, x), singleton(rk, rx))
          case Bin(rk, rx, rl@Bin(rlk, rlx, rll, rlr), rr@Bin(_, _, _, _)) =>
            if (rl.size < ratio*rr.size) Bin(rk, rx, Bin(k, x, Tip(), rl), rr)
            else Bin(rlk, rlx, Bin(k, x, Tip(), rll), Bin(rk, rx, rlr, rr))
        }
      case Bin(lk, lx, ll, lr) =>
        r match {
          case Tip() => (ll, lr) match {
            case (Tip(), Tip()) =>
              Bin(k, x, l, Tip())
            case (Tip(), Bin(lrk, lrx, _, _)) =>
              Bin(lrk, lrx, singleton(lk, lx), singleton(k, x))
            case (Bin(_, _, _, _), Tip()) =>
              Bin(lk, lx, ll, singleton(k, x))
            case (Bin(_, _, _, _), Bin(lrk, lrx, lrl, lrr)) =>
              if (lr.size < ratio*ll.size) Bin(lk, lx, ll, Bin(k, x, lr, Tip()))
              else Bin(lrk, lrx, Bin(lk, lx, ll, lrl), Bin(k, x, lrr, Tip()))
          }
          case Bin(rk, rx, rl, rr) =>
            if (r.size > delta*l.size) {
              (rl, rr) match {
                case (Bin(rlk, rlx, rll, rlr), Bin(_, _, _, _)) =>
                  if (rl.size < ratio*rr.size) Bin(rk, rx, Bin(k, x, l, rl), rr)
                  else Bin(rlk, rlx, Bin(k, x, l, rll), Bin(rk, rx, rlr, rr))
                case _ =>
                  sys.error("Failure in Map.balance")
              }
            }
            else if (l.size > delta*r.size) {
              (ll, lr) match {
                case (Bin(_, _, _, _), Bin(lrk, lrx, lrl, lrr)) =>
                  if (lr.size < ratio*ll.size) Bin(lk, lx, ll, Bin(k, x, lr, r))
                  else Bin(lrk, lrx, Bin(lk, lx, ll, lrl), Bin(k, x, lrr, r))
                case _ =>
                  sys.error("Failure in Map.balance")
              }
            }
            else Bin(k, x, l, r)
        }
    }

  // balanceL is used to balance a tree when an element might have been inserted to
  // a left subtree, or when an element might have been deleted from a right subtree.
  private[scalaz] def balanceL[A, B](k: A, x: B, l: A ==>> B, r: A ==>> B): A ==>> B =
    r match {
      case Tip() =>
        l match {
          case Tip() =>
            singleton(k, x)
          case Bin(_, _, Tip(), Tip()) =>
            Bin(k, x, l, Tip())
          case Bin(lk, lx, Tip(), Bin(lrk, lrx, _, _)) =>
            Bin(lrk, lrx, singleton(lk, lx), singleton(k, x))
          case Bin(lk, lx, ll@Bin(_, _, _, _), Tip()) =>
            Bin(lk, lx, ll, singleton(k, x))
          case Bin(lk, lx, ll@Bin(_, _, _, _), lr@Bin(lrk, lrx, lrl, lrr)) =>
            if (lr.size < ratio*ll.size) Bin(lk, lx, ll, Bin(k, x, lr, Tip()))
            else Bin(lrk, lrx, Bin(lk, lx, ll, lrl), Bin(k, x, lrr, Tip()))
        }

      case Bin(_, _, _, _) =>
        l match {
          case Tip() =>
            Bin(k, x, Tip(), r)
          case Bin(lk, lx, ll, lr) =>
            if (l.size > delta*r.size) {
              (ll, lr) match {
                case (Bin(_, _, _, _), Bin(lrk, lrx, lrl, lrr)) =>
                  if (lr.size < ratio*ll.size) Bin(lk, lx, ll, Bin(k, x, lr, r))
                  else Bin(lrk, lrx, Bin(lk, lx, ll, lrl), Bin(k, x, lrr, r))
                case _ => sys.error("Failure in Map.balanceL")
              }
            } else Bin(k, x, l, r)
        }
    }

  // balanceR is used to balance a tree when a key-value might have been inserted to
  // a right subtree, or when a key-value might have been deleted from a left subtree.
  private[scalaz] def balanceR[A, B](k: A, x: B, l: A ==>> B, r: A ==>> B): A ==>> B =
    l match {
      case Tip() =>
        r match {
          case Tip() =>
            singleton(k, x)
          case Bin(_, _, Tip(), Tip()) =>
            Bin(k, x, Tip(), r)
          case Bin(rk, rx, Tip(), rr@Bin(_, _, _, _)) =>
            Bin(rk, rx, singleton(k, x), rr)
          case Bin(rk, rx, Bin(rlk, rlx, _, _), Tip()) =>
            Bin(rlk, rlx, singleton(k, x), singleton(rk, rx))
          case Bin(rk, rx, rl@Bin(rlk, rlx, rll, rlr), rr@Bin(_, _, _, _)) =>
            if (rl.size < ratio*rr.size) Bin(rk, rx, Bin(k, x, Tip(), rl), rr)
            else Bin(rlk, rlx, Bin(k, x, Tip(), rll), Bin(rk, rx, rlr, rr))
        }
      case Bin(_, _, _, _) =>
        r match {
          case Tip() =>
            Bin(k, x, l, Tip())
          case Bin(rk, rx, rl, rr) =>
            if (r.size > delta*l.size) {
              (rl, rr) match {
                case (Bin(rlk, rlx, rll, rlr), Bin(_, _, _, _)) =>
                  if (rl.size < ratio*rr.size) Bin(rk, rx, Bin(k, x, l, rl), rr)
                  else Bin(rlk, rlx, Bin(k, x, l, rll), Bin(rk, rx, rlr, rr))
                case _ => sys.error("Failure in Map.balanceR")
              }
            } else Bin(k, x, l, r)
        }
    }

  private[scalaz] def link[A, B](kx: A, x: B, l: A ==>> B, r: A ==>> B): A ==>> B =
    (l, r) match {
      case (Tip(), r) =>
        insertMin(kx, x, r)
      case (l, Tip()) =>
        insertMax(kx, x, l)
      case (Bin(ky, y, ly, ry), Bin(kz, z, lz, rz)) =>
        if (delta * l.size < r.size) balanceL(kz, z, link(kx, x, l, lz), rz)
        else if (delta * r.size < l.size) balanceR(ky, y, ly, link(kx, x, ry, r))
        else Bin(kx, x, l, r)
    }

  // insertMax and insertMin are only used for link(...)
  private def insertMax[A, B](kx: A, x: B, t: A ==>> B): A ==>> B =
    t match {
      case Tip() =>
        singleton(kx, x)
      case Bin(ky, y, l, r) =>
        balanceR(ky, y, l, insertMax(kx, x, r))
    }

  private def insertMin[A, B](kx: A, x: B, t: A ==>> B): A ==>> B =
    t match {
      case Tip() =>
        singleton(kx, x)
      case Bin(ky, y, l, r) =>
        balanceL(ky, y, insertMin(kx, x, l), r)
    }

  private[scalaz] def trim[A, B](lo: Option[A], hi: Option[A], t: A ==>> B)(implicit o: Order[A]): A ==>> B =
    (lo, hi) match {
      case (None, None) =>
        t
      case (Some(lk), None) =>
        @tailrec
        def greater(lo: A, m: A ==>> B): A ==>> B =
          m match {
            case Bin(kx, _, _, r) if o.lessThanOrEqual(kx, lo) =>
              greater(lo, r)
            case _ =>
              m
          }
        greater(lk, t)
      case (None, Some(hk)) =>
        @tailrec
        def lesser(hi: A, m: A ==>> B): A ==>> B =
          m match {
            case Bin(kx, _, l, _) if o.greaterThanOrEqual(kx, hi) =>
              lesser(hi, l)
            case _ =>
              m
          }
        lesser(hk, t)
      case (Some(lk), Some(hk)) =>
        @tailrec
        def middle(lo: A, hi: A, m: A ==>> B): A ==>> B =
          m match {
            case Bin(kx, _, _, r) if o.lessThanOrEqual(kx, lo) =>
              middle(lo, hi, r)
            case Bin(kx, _, l, _) if o.greaterThanOrEqual(kx, hi) =>
              middle(lo, hi, l)
            case _ => m
          }
        middle(lk, hk, t)
    }

  private[scalaz] def trimLookupLo[A, B](lk: A, hkOption: Option[A], t: A ==>> B)(implicit o: Order[A]): (Option[B], A ==>> B) =
    hkOption match {
      case None =>
        @tailrec
        def greater(lo: A, m: A ==>> B): (Option[B], A ==>> B) =
          m match {
            case Tip() =>
              (none, Tip())
            case Bin(kx, x, l, r) =>
              o.order(lo, kx) match {
                case LT =>
                  (l.lookup(lo), m)
                case EQ =>
                  (some(x), r)
                case GT =>
                  greater(lo, r)
              }
          }
        greater(lk, t)
      case Some(hk) =>
        @tailrec
        def middle(lo: A, hi: A, m: A ==>> B): (Option[B], A ==>> B) =
          m match {
            case Tip() =>
              (none, Tip())
            case Bin(kx, x, l, r) =>
              o.order(lo, kx) match {
                case LT if o.order(kx, hi) == LT =>
                  (l.lookup(lo), m)
                case LT =>
                  middle(lo, hi, l)
                case EQ =>
                  (some(x), lesser(hi, r))
                case GT =>
                  middle(lo, hi, r)
              }
          }
        @tailrec
        def lesser(hi: A, m: A ==>> B): A ==>> B =
          m match {
            case Bin(k, _, l, _) if o.greaterThanOrEqual(k, hi) =>
              lesser(hi, l)
            case _ =>
              m
          }
        middle(lk, hk, t)
    }
}
