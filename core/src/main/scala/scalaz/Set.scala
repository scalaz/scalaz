package scalaz

import Ordering._
import std.option._
import syntax.std.option._

sealed abstract class Set[A] {
  import Set._

  val size: Int

  // -- * Query
  def isEmpty =
    this match {
      case Tip() => true
      case Bin(_, _, _) => false
    }

  def member(x: A)(implicit o: Order[A]): Boolean =
    this match {
      case Tip() => false
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT => l.member(x)
          case GT => r.member(x)
          case EQ => true
        }
    }

  def contains(x: A)(implicit o: Order[A]) =
    member(x)

  def notMember(x: A)(implicit o: Order[A]) =
    !member(x)

  def lookupLT(x: A)(implicit o: Order[A]): Option[A] = {
    def withBest(x: A, best: A, t: Set[A]): Option[A] =
      t match {
        case Tip() =>
          best.some
        case Bin(y, l, r) =>
          if (o.lessThanOrEqual(x, y)) withBest(x, best, l) else withBest(x, y, r)
      }

    this match {
      case Tip() =>
        none
      case Bin(y, l, r) =>
        if (o.lessThanOrEqual(x, y)) l.lookupLT(x) else withBest(x, y, r)

    }
  }

  def lookupGT(x: A)(implicit o: Order[A]): Option[A] = {
    def withBest(x: A, best: A, t: Set[A]): Option[A] =
      t match {
        case Tip() =>
          best.some
        case Bin(y, l, r) =>
          if (o.lessThan(x, y)) withBest(x, best, l) else withBest(x, best, r)
      }

    this match {
      case Tip() =>
        none
      case Bin(y, l, r) =>
        if (o.lessThan(x, y)) withBest(x, y, l) else r.lookupGT(x)
    }
  }

  def lookupLE(x: A)(implicit o: Order[A]): Option[A] = {
    def withBest(x: A, best: A, t: Set[A]): Option[A] =
      t match {
        case Tip() =>
          best.some
        case Bin(y, l, r) =>
          o.order(x, y) match {
            case LT =>
              withBest(x, best, l)
            case EQ =>
              y.some
            case GT =>
              withBest(x, y, r)
          }
      }

    this match {
      case Tip() =>
        none
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            l.lookupLE(x)
          case EQ =>
            y.some
          case GT =>
            withBest(x, y, r)
        }
    }
  }

  def lookupGE(x: A)(implicit o: Order[A]): Option[A] = {
    def withBest(x: A, best: A, t: Set[A]): Option[A] =
      t match {
        case Tip() =>
          best.some
        case Bin(y, l, r) =>
          o.order(x, y) match {
            case LT =>
              withBest(x, y, l)
            case EQ =>
              y.some
            case GT =>
              withBest(x, best, r)
          }
      }

    this match {
      case Tip() =>
        none
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            withBest(x, y, l)
          case EQ =>
            y.some
          case GT =>
            r.lookupGE(x)
        }
    }
  }

  def isSubsetOf(other: Set[A])(implicit o: Order[A]) =
    (this.size <= other.size) && this.isSubsetOfX(other)

  private def isSubsetOfX(other: Set[A])(implicit o: Order[A]): Boolean =
    (this, other) match {
      case (_, Tip()) =>
        true
      case (Tip(), _) =>
        false
      case (Bin(x, l, r), t) =>
        val (lt,found,gt) = t.splitMember(x)
        found && l.isSubsetOfX(lt) && r.isSubsetOfX(gt)
    }

  def isProperSubsetOf(other: Set[A])(implicit o: Order[A]) =
    (this.size < other.size) && this.isSubsetOf(other)

  // -- * Construction
  def insert(x: A)(implicit o: Order[A]): Set[A] =
    this match {
      case Tip() => singleton(x)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT => balanceL(y, l.insert(x), r)
          case GT => balanceR(y, l, r.insert(x))
          case EQ => Bin(x, l, r)
        }
    }

  def delete(x: A)(implicit o: Order[A]): Set[A] =
    this match {
      case Tip() =>
        Tip()
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            balanceR(y, l.delete(x), r)
          case GT =>
            balanceL(y, l, r.delete(x))
          case EQ =>
            glue(l, r)
        }
    }

  // -- * Combine
  def union(other: Set[A])(implicit o: Order[A]): Set[A] = {
    def hedgeUnion(blo: Option[A], bhi: Option[A], t1: Set[A], t2: Set[A])(implicit o: Order[A]): Set[A] =
      (t1, t2) match {
        case (t1, Tip()) =>
          t1
        case (Tip(), Bin(x, l, r)) =>
          join(x, l.filterGt(blo), r.filterLt(bhi))
        case (_, Bin(x, Tip(), Tip())) =>
          t1.insertR(x)
        case (Bin(x, l, r), _) =>
          val bmi = x.some
          join(x, hedgeUnion(blo, bmi, l, t2.trim(blo, bmi)), hedgeUnion(bmi, bhi, r, t2.trim(bmi, bhi)))
      }

    (this, other) match {
      case (Tip(), t2) =>
        t2
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeUnion(none, none, t1, t2)
    }
  }

  private def insertR(x: A)(implicit o: Order[A]): Set[A] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            balanceL(y, l.insertR(x), r)
          case GT =>
            balanceR(y, l, r.insertR(x))
          case EQ =>
            this
        }
    }

  def difference(other: Set[A])(implicit o: Order[A]): Set[A] = {
    def hedgeDiff(blo: Option[A], bhi: Option[A], t1: Set[A], t2: Set[A]): Set[A] =
      (t1, t2) match {
        case (Tip(), _) =>
          Tip()
        case (Bin(x, l, r), Tip()) =>
          join(x, l.filterGt(blo), r.filterLt(bhi))
        case (t, Bin(x, l, r)) =>
          val bmi = x.some
          hedgeDiff(blo, bmi, t.trim(blo, bmi), l) merge hedgeDiff(bmi, bhi, t.trim(bmi, bhi), r)
      }

    (this, other) match {
      case (Tip(), _) =>
        Tip()
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeDiff(None, None, t1, t2)
    }
  }

  // -- * Operators
  def \\ (other: Set[A])(implicit o: Order[A]) =
    difference(other)

  def intersection(other: Set[A])(implicit o: Order[A]) = {
    def hedgeInt(blo: Option[A], bhi: Option[A], t1: Set[A], t2: Set[A]): Set[A] =
      (t1, t2) match {
        case (_, Tip()) =>
          t2
        case (Tip(), _) =>
          t1
        case (Bin(x, l, r), t2) =>
          val bmi = x.some
          val l2 = hedgeInt(blo, bmi, l, t2.trim(blo, bmi))
          val r2 = hedgeInt(bmi, bhi, r, t2.trim(bmi, bhi))
          if (t2.member(x)) join(x, l2, r2) else l2 merge r2
      }

    (this, other) match {
      case (Tip(), _) =>
        this
      case (_, Tip()) =>
        other
      case (t1, t2) =>
        hedgeInt(None, None, t1, t2)
    }
  }

  // -- * Filter
  def filter(p: A => Boolean): Set[A] =
    this match {
      case Tip() => this
      case Bin(x, l, r) =>
        if (p(x)) join(x, l.filter(p), r.filter(p)) else l.filter(p) merge r.filter(p)
    }

  def partition(p: A => Boolean): (Set[A], Set[A]) =
    this match {
      case Tip() =>
        (this, this)
      case Bin(x, l, r) =>
        val (l1, l2) = l.partition(p)
        val (r1, r2) = r.partition(p)
        if (p(x)) (join(x, l1, r1), l2 merge r2) else (l1 merge r1, join(x, l2, r2))
    }

  def split(x: A)(implicit o: Order[A]): (Set[A], Set[A]) =
    this match {
      case Tip() =>
        (this, this)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            val (lt, gt) = l.split(x)
            (lt, join(y, gt, r))
          case GT =>
            val (lt, gt) = r.split(x)
            (join(y, l, lt), gt)
          case EQ =>
            (l, r)
        }
    }

  def splitMember(x: A)(implicit o: Order[A]): (Set[A], Boolean, Set[A]) =
    this match {
      case Tip() =>
        (this, false, this)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT =>
            val (lt, found, gt) = l.splitMember(x)
            (lt, found, join(y, gt, r))
          case GT =>
            val (lt, found, gt) = r.splitMember(x)
            (join(y, l, lt), found, gt)
          case EQ =>
            (l, true, r)
        }
    }

  // -- * Map
  def map[B: Order](f: A => B) =
    fromList(toList.map(f))
  // , mapMonotonic

  // -- * Folds
  def foldRight[B](z: B)(f: (A, B) => B): B =
    this match {
      case Tip() => z
      case Bin(x, l ,r) => l.foldRight(f(x, r.foldRight(z)(f)))(f)
    }

  def foldr[B](z: B)(f: (A, B) => B): B =
    foldRight(z)(f)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case Tip() => z
      case Bin(x, l, r) =>
        r.foldLeft(f(l.foldLeft(z)(f), x))(f)
    }

  def foldl[B](z: B)(f: (B, A) => B): B =
    foldLeft(z)(f)

  // -- * Min\/Max
  def findMin: Option[A] =
    this match {
      case Tip() => none
      case Bin(x, Tip(), _) => x.some
      case Bin(_, l, _) => l.findMin
    }

  def findMax: Option[A] =
    this match {
      case Tip() => none
      case Bin(x, _, Tip()) => x.some
      case Bin(_, _, r) => r.findMax
    }

  def deleteMin: Set[A] =
    this match {
      case Bin(_, Tip(), r) => r
      case Bin(x, l, r) => balanceR(x, l.deleteMin, r)
      case Tip() => Tip()
    }

  def deleteMax: Set[A] =
    this match {
      case Bin(_, l, Tip()) => l
      case Bin(x, l, r) => balanceL(x, l, r.deleteMax)
      case Tip() => Tip()
    }

  import std.tuple._
  def deleteFindMin: Option[(A, Set[A])] =
    this match {
      case Bin(x, Tip(), r) => (x, r).some
      case Bin(x, l, r) =>
        l.deleteFindMin.map(a => Functor[({ type X[Y] = Tuple2[A, Y] })#X].map(a)(b => balanceR(x, b, r)))
      case Tip() => none
    }

  def deleteFindMax: Option[(A, Set[A])] =
    this match {
      case Bin(x, l, Tip()) => (x, l).some
      case Bin(x, l, r) =>
        r.deleteFindMax.map(a => Functor[({ type X[Y] = Tuple2[A, Y] })#X].map(a)(b => balanceL(x, b, r)))
      case Tip() => none
    }

  def minView: Option[(A, Set[A])] =
    this match {
      case Tip() => none
      case x => x.deleteFindMin
    }

  def maxView: Option[(A, Set[A])] =
    this match {
      case Tip() => none
      case x => x.deleteFindMax
    }

  // -- * Conversion

  // -- ** List
  def elems =
    toAscList

  def toList =
    toAscList

  // -- ** Ordered list
  def toAscList =
    foldRight(List.empty[A])(_ :: _)

  def toDescList =
    foldLeft(List.empty[A])((a, b) => b :: a)


            // -- * Debugging
            // , showTree
            // , showTreeWith
            // , valid

            // -- Internals (for testing)
            // , balanced

  private def glue[A](l: Set[A], r: Set[A]): Set[A] =
    (l, r) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (_, _) =>
        if (l.size > r.size) {
          l.deleteFindMax.map(b => balanceR(b._1, b._2, r)).get
        } else {
          r.deleteFindMin.map(b => balanceL(b._1, l, b._2)).get
        }
    }

  private def join[A](x: A, l: Set[A], r: Set[A]): Set[A] =
    (l, r) match {
      case (Tip(), r) => r.insertMin(x)
      case (l, Tip()) => l.insertMin(x)
      case (Bin(y, ly, ry), Bin(z, lz, rz)) =>
        if (delta*l.size < r.size) balanceL(z, join(x, l, lz), rz)
        else if (delta*r.size < l.size) balanceR(y, ly, join(x, ry, r))
        else Bin(x, l, r)
    }

  private def insertMax(x: A): Set[A] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        balanceR(y, l, r.insertMax(x))
    }

  private def insertMin(x: A): Set[A] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        balanceL(y, l.insertMin(x), r)
    }

  private def merge(other: Set[A]): Set[A] =
    (this, other) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (l@Bin(x, lx, rx), r@Bin(y, ly, ry)) =>
        if (delta*l.size < r.size) balanceL(y, l.merge(ly), ry)
        else if (delta*r.size < l.size) balanceR(x, lx, rx.merge(r))
        else glue(l, r)
    }

  def trim(a: Option[A], b: Option[A])(implicit o: Order[A]): Set[A] =
    (a, b) match {
      case (None, None) =>
        this
      case (Some(lx), None) =>
        def greater(lo: A, t: Set[A]): Set[A] =
          t match {
            case Bin(x, _, r) => if (o.lessThanOrEqual(x, lo)) greater(lo, r) else t
            case _ => t
          }
        greater(lx, this)
      case (None, Some(hx)) =>
        def lesser(hi: A, t: Set[A]): Set[A] =
          t match {
            case Bin(x, l, _) => if (o.greaterThanOrEqual(x, hi)) lesser(hi, l) else t
            case _ => t
          }
        lesser(hx, this)
      case (Some(lx), Some(rx)) =>
        def middle(lo: A, hi: A, t: Set[A]): Set[A] =
          t match {
            case Bin(x, l, r) =>
              if (o.lessThanOrEqual(x, lo)) middle(lo, hi, r)
              else if (o.greaterThanOrEqual(x, hi)) middle(lo, hi, l)
              else t
            case _ => t
          }
        middle(lx, rx, this)
    }

  def filterGt(a: Option[A])(implicit o: Order[A]): Set[A] =
    a.cata(s => this match {
      case Tip() => Set.empty
      case Bin(x, l, r) =>
        o.order(s, x) match {
          case LT => join(x, l.filterGt(a), r)
          case EQ => r
          case GT => r.filterGt(a)
        }
    }, this)

  def filterLt(a: Option[A])(implicit o: Order[A]): Set[A] =
    a.cata(s => this match {
      case Tip() => Set.empty
      case Bin(x, l, r) =>
        o.order(x, s) match {
          case LT => join(x, l, r.filterLt(a))
          case EQ => l
          case GT => l.filterLt(a)
        }
    }, this)
}

object Set extends SetInstances with SetFunctions {
  private[scalaz] case class Tip[A]() extends Set[A] {
    val size = 0
  }

  private[scalaz] final case class Bin[A](a: A, l: Set[A], r: Set[A]) extends Set[A] {
    val size = l.size + r.size + 1
  }
}

trait SetInstances {
  import Set._
  import std.list._

  implicit def setEqual[A: Equal]: Equal[Set[A]] = new Equal[Set[A]] {
    import syntax.equal._

    def equal(a1: Set[A], a2: Set[A]): Boolean =
      (a1.size == a2.size) && (a1.toAscList === a2.toAscList)
  }

  implicit def setOrder[A: Order]: Order[Set[A]] = new Order[Set[A]] {
    def order(x: Set[A], y: Set[A]) =
      implicitly[Order[List[A]]].order(x.toAscList, y.toAscList)
  }

  implicit def setShow[A: Show]: Show[Set[A]] = new Show[Set[A]] {
    override def shows(f: Set[A]) =
      f.toAscList.mkString("Set(", ",", ")")
  }

  implicit def setMonoid[A: Order]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def zero: Set[A] =
      empty[A]

    def append(a: Set[A], b: => Set[A]): Set[A] =
      a union b
  }

  implicit def setFoldable: Foldable[Set] = new Foldable[Set] {
    def foldMap[A, B](fa: Set[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa match {
        case Tip() =>
          F.zero
        case Bin(x, l, r) =>
          F.append(F.append(foldMap(l)(f), f(x)), foldMap(r)(f))
      }

    def foldRight[A, B](fa: Set[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)((a, b) => f(a, b))
  }
}

trait SetFunctions {
  import Set._

  final def empty[A]: Set[A] =
    Tip()

  final def singleton[A](x: A): Set[A] =
    Bin(x, Tip(), Tip())

  final def fromList[A](xs: List[A])(implicit o: Order[A]): Set[A] =
    xs.foldLeft(empty[A])((a, b) => a insert b)

  final def unions[A](xs: List[Set[A]])(implicit o: Order[A]): Set[A] =
    xs.foldLeft(Set.empty[A])(_ union _)

  final def fromAscList[A](xs: List[A])(implicit o: Order[A]): Set[A] =
    fromDistinctAscList(xs.distinct)

  final def fromDistinctAscList[A](xs: List[A])(implicit o: Order[A]): Set[A] = {
    //def createR
    def create(c: A => A => A, n: Int, xs: List[A]): Set[A] =
      if (n == 0)
        Tip()
      else if (n == 5) {
        xs match {
          case x1 :: x2 :: x3 :: x4 :: x5 :: xx => Bin(x4, Bin(x2, singleton(x1), singleton(x3)), singleton(x5))
          case _ => sys.error("error in create 5")
        }
      } else {
        empty[A]
      }

    create(Function.const, xs.length, xs)
  }


//   fromDistinctAscList :: [a] -> Set a
//   fromDistinctAscList xs
//   = create const (length xs) xs
//   where
//   -- 1) use continutations so that we use heap space instead of stack space.
//   -- 2) special case for n==5 to create bushier trees.
//     create c 0 xs' = c Tip xs'
//     create c 5 xs' = case xs' of
//       (x1:x2:x3:x4:x5:xx) -> c (bin x4 (bin x2 (singleton x1) (singleton x3)) (singleton x5)) xx
//       _ -> error "fromDistinctAscList create 5"
//     create c n xs' = seq nr $ create (createR nr c) nl xs'
//         where nl = n `div` 2
//               nr = n - nl - 1

// createR n c l (x:ys) = create (createB l x c) n ys
// createR _ _ _ []     = error "fromDistinctAscList createR []"
// createB l x c r zs   = c (bin x l r) zs

  private[scalaz] val delta = 3
  private[scalaz] val ratio = 2

  private[scalaz] def balanceL[A](x: A, l: Set[A], r: Set[A]): Set[A] =
    r match {
      case Tip() =>
        l match {
          case Tip() =>
            singleton(x)
          case Bin(_, Tip(), Tip()) =>
            Bin(x, l, Tip())
          case Bin(lx, Tip(), Bin(lrx, _, _)) =>
            Bin(lrx, singleton(lx), singleton(x))
          case Bin(lx, ll@Bin(_, _, _), Tip()) =>
            Bin(lx, ll, singleton(x))
          case Bin(lx, ll@Bin(_, _, _), lr@Bin(lrx, lrl, lrr)) =>
            if (lr.size < ratio*ll.size) Bin(lx, ll, Bin(x, lr, Tip()))
            else Bin(lrx, Bin(lx, ll, lrl), Bin(x, lrr, Tip()))
        }
      case Bin(_, _, _) =>
        l match {
          case Tip() =>
            Bin(x, Tip(), r)
          case Bin(lx, ll, lr) =>
            if (l.size > delta*r.size) {
              (ll, lr) match {
                case (Bin(_, _, _), Bin(lrx, lrl, lrr)) =>
                  if (lr.size < ratio*ll.size) Bin(lx, ll, Bin(x, lr, r))
                  else Bin(lrx, Bin(lx, ll, lrl), Bin(x, lrr, r))
                case _ => sys.error("Failure in Set.balanceL")
              }
            } else Bin(x, l, r)
        }
    }

  private[scalaz] def balanceR[A](x: A, l: Set[A], r: Set[A]): Set[A] =
    l match {
      case Tip() =>
        r match {
          case Tip() =>
            singleton(x)
          case Bin(_, Tip(), Tip()) =>
            Bin(x, Tip(), r)
          case Bin(rx, Tip(), rr@Bin(_, _, _)) =>
            Bin(rx, singleton(x), rr)
          case Bin(rx, Bin(rlx, _, _), Tip()) =>
            Bin(rlx, singleton(x), singleton(rx))
          case Bin(rx, rl@Bin(rlx, rll, rlr), rr@Bin(_, _, _)) =>
            if (rl.size < ratio*rr.size) Bin(rx, Bin(x, Tip(), rl), rr)
            else Bin(rlx, Bin(x, Tip(), rll), Bin(rx, rlr, rr))
        }
      case Bin(_, _, _) =>
        r match {
          case Tip() =>
            Bin(x, l, Tip())
          case Bin(rx, rl, rr) =>
            if (r.size > delta*l.size) {
              (rl, rr) match {
                case (Bin(rlx, rll, rlr), Bin(_, _, _)) =>
                  if (rl.size < ratio*rr.size) Bin(rx, Bin(x, l, rl), rr)
                  else Bin(rlx, Bin(x, l, rll), Bin(rx, rlr, rr))
                case _ => sys.error("Failure in Set.balanceR")
              }
            } else Bin(x, l, r)
        }
    }
}
