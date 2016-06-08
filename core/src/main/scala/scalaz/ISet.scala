package scalaz

import annotation.tailrec
import Ordering._
import std.option._

/**
 * @see [[http://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Set.html]]
 * @see [[https://github.com/haskell/containers/blob/v0.5.7.1/Data/Set/Base.hs]]
 */
sealed abstract class ISet[A] {
  import ISet._

  val size: Int

  // -- * Query
  final def isEmpty: Boolean =
    this match {
      case Tip() => true
      case Bin(_, _, _) => false
    }

  @tailrec
  final def member(x: A)(implicit o: Order[A]): Boolean =
    this match {
      case Tip() => false
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT => l.member(x)
          case GT => r.member(x)
          case EQ => true
        }
    }

  /** Alias for member */
  final def contains(x: A)(implicit o: Order[A]): Boolean =
    member(x)

  final def notMember(x: A)(implicit o: Order[A]): Boolean =
    !member(x)

  @tailrec
  final def lookupLT(x: A)(implicit o: Order[A]): Option[A] = {
    @tailrec
    def withBest(x: A, best: A, t: ISet[A]): Option[A] =
      t match {
        case Tip() =>
          some(best)
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

  @tailrec
  final def lookupGT(x: A)(implicit o: Order[A]): Option[A] = {
    @tailrec
    def withBest(x: A, best: A, t: ISet[A]): Option[A] =
      t match {
        case Tip() =>
          some(best)
        case Bin(y, l, r) =>
          if (o.lessThan(x, y)) withBest(x, y, l) else withBest(x, best, r)
      }

    this match {
      case Tip() =>
        none
      case Bin(y, l, r) =>
        if (o.lessThan(x, y)) withBest(x, y, l) else r.lookupGT(x)
    }
  }

  @tailrec
  final def lookupLE(x: A)(implicit o: Order[A]): Option[A] = {
    @tailrec
    def withBest(x: A, best: A, t: ISet[A]): Option[A] =
      t match {
        case Tip() =>
          some(best)
        case Bin(y, l, r) =>
          o.order(x, y) match {
            case LT =>
              withBest(x, best, l)
            case EQ =>
              some(y)
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
            some(y)
          case GT =>
            withBest(x, y, r)
        }
    }
  }

  @tailrec
  final def lookupGE(x: A)(implicit o: Order[A]): Option[A] = {
    @tailrec
    def withBest(x: A, best: A, t: ISet[A]): Option[A] =
      t match {
        case Tip() =>
          some(best)
        case Bin(y, l, r) =>
          o.order(x, y) match {
            case LT =>
              withBest(x, y, l)
            case EQ =>
              some(y)
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
            some(y)
          case GT =>
            r.lookupGE(x)
        }
    }
  }

  final def isSubsetOf(other: ISet[A])(implicit o: Order[A]): Boolean =
    (this.size <= other.size) && this.isSubsetOfX(other)

  private def isSubsetOfX(other: ISet[A])(implicit o: Order[A]): Boolean =
    (this, other) match {
      case (Tip(), _) =>
        true
      case (_, Tip()) =>
        false
      case (Bin(x, l, r), t) =>
        val (lt,found,gt) = t.splitMember(x)
        found && l.isSubsetOfX(lt) && r.isSubsetOfX(gt)
    }

  final def isProperSubsetOf(other: ISet[A])(implicit o: Order[A]): Boolean =
    (this.size < other.size) && this.isSubsetOf(other)

  // -- * Construction
  final def insert(x: A)(implicit o: Order[A]): ISet[A] =
    this match {
      case Tip() => singleton(x)
      case Bin(y, l, r) =>
        o.order(x, y) match {
          case LT => balanceL(y, l.insert(x), r)
          case GT => balanceR(y, l, r.insert(x))
          case EQ => Bin(x, l, r)
        }
    }

  final def delete(x: A)(implicit o: Order[A]): ISet[A] =
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
  final def union(other: ISet[A])(implicit o: Order[A]): ISet[A] = {
    def hedgeUnion(blo: Option[A], bhi: Option[A], t1: ISet[A], t2: ISet[A])(implicit o: Order[A]): ISet[A] =
      (t1, t2) match {
        case (t1, Tip()) =>
          t1
        case (Tip(), Bin(x, l, r)) =>
          join(x, l.filterGt(blo), r.filterLt(bhi))
        case (_, Bin(x, Tip(), Tip())) =>
          t1.insertR(x)
        case (Bin(x, l, r), _) =>
          val bmi = some(x)
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

  private def insertR(x: A)(implicit o: Order[A]): ISet[A] =
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

  final def difference(other: ISet[A])(implicit o: Order[A]): ISet[A] = {
    def hedgeDiff(blo: Option[A], bhi: Option[A], t1: ISet[A], t2: ISet[A]): ISet[A] =
      (t1, t2) match {
        case (Tip(), _) =>
          Tip()
        case (Bin(x, l, r), Tip()) =>
          join(x, l.filterGt(blo), r.filterLt(bhi))
        case (t, Bin(x, l, r)) =>
          val bmi = some(x)
          hedgeDiff(blo, bmi, t.trim(blo, bmi), l) merge hedgeDiff(bmi, bhi, t.trim(bmi, bhi), r)
      }

    (this, other) match {
      case (Tip(), _) =>
        Tip()
      case (t1, Tip()) =>
        t1
      case (t1, t2) =>
        hedgeDiff(none, none, t1, t2)
    }
  }

  // -- * Operators
  final def \\ (other: ISet[A])(implicit o: Order[A]): ISet[A] =
    difference(other)

  final def intersection(other: ISet[A])(implicit o: Order[A]): ISet[A] = {
    def hedgeInt(blo: Option[A], bhi: Option[A], t1: ISet[A], t2: ISet[A]): ISet[A] =
      (t1, t2) match {
        case (_, Tip()) =>
          t2
        case (Tip(), _) =>
          t1
        case (Bin(x, l, r), t2) =>
          val bmi = some(x)
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
  final def filter(p: A => Boolean): ISet[A] =
    this match {
      case Tip() => this
      case Bin(x, l, r) =>
        if (p(x)) join(x, l.filter(p), r.filter(p)) else l.filter(p) merge r.filter(p)
    }

  final def partition(p: A => Boolean): (ISet[A], ISet[A]) =
    this match {
      case Tip() =>
        (this, this)
      case Bin(x, l, r) =>
        val (l1, l2) = l.partition(p)
        val (r1, r2) = r.partition(p)
        if (p(x)) (join(x, l1, r1), l2 merge r2) else (l1 merge r1, join(x, l2, r2))
    }

  final def split(x: A)(implicit o: Order[A]): (ISet[A], ISet[A]) =
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

  final def splitMember(x: A)(implicit o: Order[A]): (ISet[A], Boolean, ISet[A]) =
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

  final def splitRoot: List[ISet[A]] =
    this match {
      case Tip()        => List.empty[ISet[A]]
      case Bin(x, l, r) => List(l, singleton(x), r)
    }

  // -- * Index
  /** Alias for Foldable[ISet].index */
  final def elemAt(i: Int): Option[A] =
    Foldable[ISet].index(this, i)

  final def lookupIndex(x: A)(implicit o: Order[A]): Option[Int] = {
    @tailrec
    def loop(s: ISet[A], i: Int): Option[Int] =
      s match {
        case Tip() =>
          none
        case Bin(y, l, r) =>
          val sizeL = l.size
          o.order(y, x) match {
            case LT =>
              loop(r, i + sizeL + 1)
            case GT =>
              loop(l, i)
            case EQ =>
              some(i + sizeL)
          }
      }

    loop(this, 0)
  }

  final def deleteAt(i: Int): ISet[A] =
    this match {
      case Tip() =>
        Tip()
      case Bin(x, l, r) =>
        import std.anyVal._
        val sizeL = l.size
        Order[Int].order(i, sizeL) match {
          case LT =>
            balanceR(x, l.deleteAt(i), r)
          case GT =>
            balanceL(x, l, r.deleteAt(i - sizeL - 1))
          case EQ =>
            glue(l, r)
        }
    }

  /**
    * For the `Functor` composition law to hold it is important that the `Order[B]` is substitutive for the `Order[A]` –
    * that is, that the `Order[B]` should be __no stronger__, it should not distinguish two `B` instances that would
    * be considered as equal `A` instances.
    *
    * '''Note:''' this is not able to implement `Functor` due to the `Order` constraint on the destination type,
    * however it still is a functor in the mathematical sense.
    *
    * Documentation as copied from the Haskell source:
    *  {{{
    -- | /O(n*log n)/.
    -- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
    --
    -- It's worth noting that the size of the result may be smaller if,
    -- for some @(x,y)@, @x \/= y && f x == f y@
    }}}
    */
  def map[B: Order](f: A => B): ISet[B] =
    fromList(toList.map(f))

  // -- * Folds
  final def foldRight[B](z: B)(f: (A, B) => B): B =
    this match {
      case Tip() => z
      case Bin(x, l ,r) => l.foldRight(f(x, r.foldRight(z)(f)))(f)
    }

  final def foldr[B](z: B)(f: (A, B) => B): B =
    foldRight(z)(f)

  final def foldLeft[B](z: B)(f: (B, A) => B): B =
    this match {
      case Tip() => z
      case Bin(x, l, r) =>
        r.foldLeft(f(l.foldLeft(z)(f), x))(f)
    }

  final def foldl[B](z: B)(f: (B, A) => B): B =
    foldLeft(z)(f)

  // -- * Min\/Max
  @tailrec
  final def findMin: Option[A] =
    this match {
      case Tip() => none
      case Bin(x, Tip(), _) => some(x)
      case Bin(_, l, _) => l.findMin
    }

  @tailrec
  final def findMax: Option[A] =
    this match {
      case Tip() => none
      case Bin(x, _, Tip()) => some(x)
      case Bin(_, _, r) => r.findMax
    }

  final def deleteMin: ISet[A] =
    this match {
      case Bin(_, Tip(), r) => r
      case Bin(x, l, r) => balanceR(x, l.deleteMin, r)
      case Tip() => Tip()
    }

  final def deleteMax: ISet[A] =
    this match {
      case Bin(_, l, Tip()) => l
      case Bin(x, l, r) => balanceL(x, l, r.deleteMax)
      case Tip() => Tip()
    }

  // TODO: Can we make this total? or should this remain unsafe, preferring minView instead?
  final def deleteFindMin: (A, ISet[A]) =
    this match {
      case Bin(x, Tip(), r) => (x, r)
      case Bin(x, l, r) =>
        val (xm, l2) = l.deleteFindMin
        (xm, balanceR(x, l2, r))
      case Tip() => sys.error("deleteFindMin on empty ISet")
    }

  // TODO: Can we make this total? or should this remain unsafe, preferring maxView instead?
  final def deleteFindMax: (A, ISet[A]) =
    this match {
      case Bin(x, l, Tip()) => (x, l)
      case Bin(x, l, r) =>
        val (xm, r2) = r.deleteFindMax
        (xm, balanceL(x, l, r2))
      case Tip() => sys.error("deleteFindMax on empty ISet")
    }

  final def minView: Option[(A, ISet[A])] =
    this match {
      case Tip() => none
      case x => some(x.deleteFindMin)
    }

  final def maxView: Option[(A, ISet[A])] =
    this match {
      case Tip() => none
      case x => some(x.deleteFindMax)
    }

  // -- ** List
  final def elems: List[A] =
    toAscList

  final def toList: List[A] =
    toAscList

  // -- ** Ordered list
  final def toAscList: List[A] =
    foldRight(List.empty[A])(_ :: _)

  final def toDescList: List[A] =
    foldLeft(List.empty[A])((a, b) => b :: a)

  private def glue[A](l: ISet[A], r: ISet[A]): ISet[A] =
    (l, r) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (_, _) =>
        if (l.size > r.size) {
          val (m, l2) = l.deleteFindMax
          balanceR(m, l2, r)
        } else {
          val (m, r2) = r.deleteFindMin
          balanceL(m, l, r2)
        }
    }

  private def join[A](x: A, l: ISet[A], r: ISet[A]): ISet[A] =
    (l, r) match {
      case (Tip(), r) => r.insertMin(x)
      case (l, Tip()) => l.insertMax(x)
      case (Bin(y, ly, ry), Bin(z, lz, rz)) =>
        if (delta*l.size < r.size) balanceL(z, join(x, l, lz), rz)
        else if (delta*r.size < l.size) balanceR(y, ly, join(x, ry, r))
        else Bin(x, l, r)
    }

  private def insertMax(x: A): ISet[A] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        balanceR(y, l, r.insertMax(x))
    }

  private def insertMin(x: A): ISet[A] =
    this match {
      case Tip() =>
        singleton(x)
      case Bin(y, l, r) =>
        balanceL(y, l.insertMin(x), r)
    }

  protected def merge(other: ISet[A]): ISet[A] =
    (this, other) match {
      case (Tip(), r) => r
      case (l, Tip()) => l
      case (l@Bin(x, lx, rx), r@Bin(y, ly, ry)) =>
        if (delta*l.size < r.size) balanceL(y, l merge ly, ry)
        else if (delta*r.size < l.size) balanceR(x, lx, rx merge r)
        else glue(l, r)
    }

  final def trim(a: Option[A], b: Option[A])(implicit o: Order[A]): ISet[A] =
    (a, b) match {
      case (None, None) =>
        this
      case (Some(lx), None) =>
        def greater(lo: A, t: ISet[A]): ISet[A] =
          t match {
            case Bin(x, _, r) => if (o.lessThanOrEqual(x, lo)) greater(lo, r) else t
            case _ => t
          }
        greater(lx, this)
      case (None, Some(hx)) =>
        def lesser(hi: A, t: ISet[A]): ISet[A] =
          t match {
            case Bin(x, l, _) => if (o.greaterThanOrEqual(x, hi)) lesser(hi, l) else t
            case _ => t
          }
        lesser(hx, this)
      case (Some(lx), Some(rx)) =>
        def middle(lo: A, hi: A, t: ISet[A]): ISet[A] =
          t match {
            case Bin(x, l, r) =>
              if (o.lessThanOrEqual(x, lo)) middle(lo, hi, r)
              else if (o.greaterThanOrEqual(x, hi)) middle(lo, hi, l)
              else t
            case _ => t
          }
        middle(lx, rx, this)
    }

  final def filterGt(a: Option[A])(implicit o: Order[A]): ISet[A] =
    cata(a)(s => this match {
      case Tip() => ISet.empty
      case Bin(x, l, r) =>
        o.order(s, x) match {
          case LT => join(x, l.filterGt(a), r)
          case EQ => r
          case GT => r.filterGt(a)
        }
    }, this)

  final def filterLt(a: Option[A])(implicit o: Order[A]): ISet[A] =
    cata(a)(s => this match {
      case Tip() => ISet.empty
      case Bin(x, l, r) =>
        o.order(x, s) match {
          case LT => join(x, l, r.filterLt(a))
          case EQ => l
          case GT => l.filterLt(a)
        }
    }, this)

  override final def equals(other: Any): Boolean =
    other match {
      case that: ISet[A] =>
        ISet.setEqual[A](Equal.equalA).equal(this, that)
      case _ =>
        false
    }

  override final def hashCode: Int =
    toAscList.hashCode
}

sealed abstract class ISetInstances {
  import ISet._

  implicit def setEqual[A: Equal]: Equal[ISet[A]] = new ISetEqual[A] {
    def A = implicitly
  }

  implicit def setOrder[A: Order]: Order[ISet[A]] = new Order[ISet[A]] with ISetEqual[A] {
    import std.list._
    def A = implicitly

    def order(x: ISet[A], y: ISet[A]) =
      Order[List[A]].order(x.toAscList, y.toAscList)
  }

  implicit def setShow[A: Show]: Show[ISet[A]] = new Show[ISet[A]] {
    override def shows(f: ISet[A]) =
      f.toAscList.mkString("ISet(", ",", ")")
  }

  implicit def setMonoid[A: Order]: Monoid[ISet[A]] = new Monoid[ISet[A]] {
    def zero: ISet[A] =
      empty[A]

    def append(a: ISet[A], b: => ISet[A]): ISet[A] =
      a union b
  }

  implicit val setFoldable: Foldable[ISet] = new Foldable[ISet] {
    override def findLeft[A](fa: ISet[A])(f: A => Boolean) =
      fa match {
        case Bin(x, l, r) =>
          findLeft(l)(f) match {
            case a @ Some(_) =>
              a
            case None =>
              if(f(x))
                Some(x)
              else
                findLeft(r)(f)
          }
        case Tip() =>
          None
      }

    override def findRight[A](fa: ISet[A])(f: A => Boolean) =
      fa match {
        case Bin(x, l, r) =>
          findRight(r)(f) match {
            case a @ Some(_) =>
              a
            case None =>
              if(f(x))
                Some(x)
              else
                findRight(l)(f)
          }
        case Tip() =>
          None
      }

    def foldMap[A, B](fa: ISet[A])(f: A => B)(implicit F: Monoid[B]): B =
      fa match {
        case Tip() =>
          F.zero
        case Bin(x, l, r) =>
          F.append(F.append(foldMap(l)(f), f(x)), foldMap(r)(f))
      }

    def foldRight[A, B](fa: ISet[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)((a, b) => f(a, b))

    override def foldLeft[A, B](fa: ISet[A], z: B)(f: (B, A) => B) =
      fa.foldLeft(z)(f)

    override def index[A](fa: ISet[A], i: Int): Option[A] = {
      import std.anyVal._
      @tailrec def loop(a: ISet[A], b: Int): Option[A] =
        a match {
          case Bin(x, l, r) =>
            Order[Int].order(b, l.size) match {
              case Ordering.LT =>
                loop(l, b)
              case Ordering.GT =>
                loop(r, b - l.size - 1)
              case Ordering.EQ =>
                Some(x)
            }
          case Tip() =>
            None
        }

      if (i < 0 || fa.size <= i)
        None
      else
        loop(fa, i)
    }

    override def toIList[A](fa: ISet[A]) =
      fa.foldRight(IList.empty[A])(_ :: _)

    override def toList[A](fa: ISet[A]) =
      fa.toList

    override def length[A](fa: ISet[A]) =
      fa.size

    override def maximum[A: Order](fa: ISet[A]) =
      fa.findMax

    override def minimum[A: Order](fa: ISet[A]) =
      fa.findMin

    override def empty[A](fa: ISet[A]) =
      fa.isEmpty

    override def any[A](fa: ISet[A])(f: A => Boolean) =
      fa match {
        case Tip() => false
        case Bin(x, l, r) =>
          any(l)(f) || f(x) || any(r)(f)
      }

    override def all[A](fa: ISet[A])(f: A => Boolean) =
      fa match {
        case Tip() => true
        case Bin(x, l, r) =>
          all(l)(f) && f(x) && all(r)(f)
      }
  }
}

object ISet extends ISetInstances {
  final def empty[A]: ISet[A] =
    Tip()

  final def singleton[A](x: A): ISet[A] =
    Bin(x, Tip(), Tip())

  final def fromList[A](xs: List[A])(implicit o: Order[A]): ISet[A] =
    xs.foldLeft(empty[A])((a, b) => a insert b)

  final def fromFoldable[F[_], A](xs: F[A])(implicit F: Foldable[F], o: Order[A]): ISet[A] =
    F.foldLeft(xs, empty[A])((a, b) => a insert b)

  final def unions[A](xs: List[ISet[A]])(implicit o: Order[A]): ISet[A] =
    xs.foldLeft(ISet.empty[A])(_ union _)

  private[scalaz] final val delta = 3
  private[scalaz] final val ratio = 2

  private[scalaz] def balanceL[A](x: A, l: ISet[A], r: ISet[A]): ISet[A] =
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
                case _ => sys.error("Failure in ISet.balanceL")
              }
            } else Bin(x, l, r)
        }
    }

  private[scalaz] def balanceR[A](x: A, l: ISet[A], r: ISet[A]): ISet[A] =
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
                case _ => sys.error("Failure in ISet.balanceR")
              }
            } else Bin(x, l, r)
        }
    }

  private[scalaz] abstract case class Tip[A] private() extends ISet[A] {
    val size = 0
  }
  private[scalaz] object Tip extends Tip[Nothing] {
    def apply[A](): ISet[A] = this.asInstanceOf[ISet[A]]
  }

  private[scalaz] final case class Bin[A](a: A, l: ISet[A], r: ISet[A]) extends ISet[A] {
    val size = l.size + r.size + 1
  }
}

private sealed trait ISetEqual[A] extends Equal[ISet[A]] {
  import std.list._
  implicit def A: Equal[A]

  override final def equal(a1: ISet[A], a2: ISet[A]) =
    (a1.size == a2.size) && Equal[List[A]].equal(a1.toAscList, a2.toAscList)
}
