package scalaz

/**
 * @see [[https://hackage.haskell.org/package/containers-0.5.7.1/docs/mini_Data-Map-Strict.html]]
 * @see [[https://github.com/haskell/containers/blob/v0.5.7.1/Data/Map/Base.hs]]
 */
import Ordering.{ EQ, LT, GT }

import std.anyVal._
import Maybe.{ Empty, Just, just }

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

  def filterM[F[_]](f: B => F[Boolean])(implicit F: Applicative[F], O:Order[A]):F[A ==>> B] = this match {
    case Tip() => F.pure(empty)
    case Bin(kx, x, l ,r) =>
      F.ap(F.tuple2(l.filterM(f),r.filterM(f)))(
        F.map(f(x))(b => (tpl:(A ==>>B, A ==>> B)) => if (b) Bin(kx, x, tpl._1, tpl._2) else tpl._1.union(tpl._2))
      )
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
    updateWithKey(k, (a, b) => just(f(a, b)))

  /** updates or removes a value - O(log n)
   *
   * if `f` returns `None`, then the key is removed from the map */
  def update(k: A, f: B => Maybe[B])(implicit o: Order[A]): A ==>> B =
    updateWithKey(k, (_, x) => f(x))

  /** like [[update]] but with the key available in the update function - O(log n) */
  def updateWithKey(k: A, f: (A, B) => Maybe[B])(implicit o: Order[A]): A ==>> B =
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
              case Just(v) =>
                Bin(kx, v, l, r)
              case Empty() =>
                glue(l, r)
            }
        }
    }

  /** looks up a key and updates its value - O(log n)
   *
   * Similar to [[updateWithKey]] but also returns the value. If the value was updated, returns the
   * new value. If the value was deleted, returns the old value. */
  def updateLookupWithKey(k: A, f: (A, B) => Maybe[B])(implicit o: Order[A]): (Maybe[B], A ==>> B) =
    this match {
      case Tip() =>
        (Maybe.empty, empty)
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
              case Just(xx) =>
                (just(xx), Bin(kx, xx, l, r))
              case Empty() =>
                (just(x), glue(l, r))
            }
        }
    }

  def alter(k: A, f: Maybe[B] => Maybe[B])(implicit o: Order[A]): A ==>> B =
    this match {
      case Tip() =>
        f(Maybe.empty) match {
          case Empty() =>
            empty
          case Just(x) =>
            singleton(k, x)
        }

      case Bin(kx, x, l,r) =>
        o.order(k, kx) match {
          case LT =>
            balance(kx, x, l.alter(k, f), r)
          case GT =>
            balance(kx, x, l, r.alter(k, f))
          case EQ =>
            f(just(x)) match {
              case Empty() =>
                glue(l, r)
              case Just(xx) =>
                Bin(kx, xx, l, r)
            }
        }
    }

  @tailrec
  final def lookup(k: A)(implicit n: Order[A]): Maybe[B] =
    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l, r) =>
        n.order(k, kx) match {
          case LT =>
            l.lookup(k)
          case GT =>
            r.lookup(k)
          case EQ =>
            just(x)
        }
    }

  @tailrec
  final def lookupAssoc(k: A)(implicit n: Order[A]): Maybe[(A, B)] =
    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l, r) =>
        n.order(k, kx) match {
          case LT =>
            l.lookupAssoc(k)
          case GT =>
            r.lookupAssoc(k)
          case EQ =>
            just((kx, x))
        }
    }

  @tailrec
  final def lookupLT(k: A)(implicit o: Order[A]): Maybe[(A, B)] = {
    @tailrec
    def goSome(kx: A, x: B, t: A ==>> B): Maybe[(A, B)] =
      t match {
        case Tip() =>
          just((kx, x))
        case Bin(ky, y, l, r) =>
          if (o.lessThanOrEqual(k, ky)) goSome(kx, x, l) else goSome(ky, y, r)
      }

    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l, r) =>
        if (o.lessThanOrEqual(k, kx)) l.lookupLT(k) else goSome(kx, x, r)
    }
  }

  @tailrec
  final def lookupGT(k: A)(implicit o: Order[A]): Maybe[(A, B)] = {
    @tailrec
    def goSome(kx: A, x: B, t: A ==>> B): Maybe[(A, B)] =
      t match {
        case Tip() =>
          just((kx, x))
        case Bin(ky, y, l, r) =>
          if (o.greaterThanOrEqual(k, ky)) goSome(kx, x, r) else goSome(ky, y, l)
      }

    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l, r) =>
        if (o.greaterThanOrEqual(k, kx)) r.lookupGT(k) else goSome(kx, x, l)
    }
  }

  @tailrec
  final def lookupLE(k: A)(implicit o: Order[A]): Maybe[(A, B)] = {
    @tailrec
    def goSome(kx: A, x: B, t: A ==>> B): Maybe[(A, B)] =
      t match {
        case Tip() =>
          just((kx, x))
        case Bin(ky, y, l, r) =>
          o.order(k, ky) match {
            case LT =>
              goSome(kx, x, l)
            case EQ =>
              just((ky, y))
            case GT =>
              goSome(ky, y, r)
          }
      }

    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            l.lookupLE(k)
          case EQ =>
            just((kx, x))
          case GT =>
            goSome(kx, x, r)
        }
    }
  }

  @tailrec
  final def lookupGE(k: A)(implicit o: Order[A]): Maybe[(A, B)] = {
    @tailrec
    def goSome(kx: A, x: B, t: A ==>> B): Maybe[(A, B)] =
      t match {
        case Tip() =>
          just((kx, x))
        case Bin(ky, y, l, r) =>
          o.order(k, ky) match {
            case LT =>
              goSome(ky, y, l)
            case EQ =>
              just((ky, y))
            case GT =>
              goSome(kx, x, r)
          }
      }

    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            goSome(kx, x, l)
          case EQ =>
            just((kx, x))
          case GT =>
            r.lookupGE(k)
        }
    }
  }

  def values: IList[B] =
    foldrWithKey(IList.empty[B])((_, x, xs) => x :: xs)

  def keys: IList[A] =
    foldrWithKey(IList.empty[A])((x, _, xs) => x :: xs)

  def keySet: ISet[A] = this match {
    case Tip()        => ISet.Tip[A]
    case Bin(k,v,l,r) => ISet.Bin(k,l.keySet,r.keySet)
  }

  def toList: List[(A, B)] =
    toAscList

  def toIList: IList[(A, B)] =
    toAscIList

  def toAscList: List[(A, B)] =
    foldrWithKey(List.empty[(A, B)])((k, x, xs) => (k, x) :: xs)

  def toAscIList: IList[(A, B)] =
    foldrWithKey(IList.empty[(A, B)])((k, x, xs) => (k, x) :: xs)

  def toDescList: List[(A, B)] =
    foldlWithKey(List.empty[(A, B)])((xs, k, x) => (k, x) :: xs)

  def toDescIList: IList[(A, B)] =
    foldlWithKey(IList.empty[(A, B)])((xs, k, x) => (k, x) :: xs)

  def member(k: A)(implicit n: Order[A]): Boolean =
    lookup(k)(n).isJust

  def notMember(k: A)(implicit n: Order[A]): Boolean =
    !member(k)

  def lookupIndex(k: A)(implicit o: Order[A]): Maybe[Int] = {
    @tailrec
    def go(n: Int, m: A ==>> B): Maybe[Int] =
      m match {
        case Tip() =>
          Maybe.empty
        case Bin(kx, x, l, r) =>
          o.order(k, kx) match {
            case LT =>
              go(n, l)
            case GT =>
              go(n + l.size + 1, r)
            case EQ =>
              just((n + l.size))
          }
      }

    go(0, this)
  }

  @tailrec
  final def elemAt(i: Int): Maybe[(A, B)] =
    this match {
      case Tip() =>
        Maybe.empty
      case Bin(kx, x, l ,r) =>
        implicitly[Order[Int]].order(i, l.size) match {
          case LT =>
            l.elemAt(i)
          case GT =>
            r.elemAt(i - l.size - 1)
          case EQ =>
            just((kx, x))
        }
    }

  // TODO: This should be a total function
  def updateAt(i: Int, f: (A, B) => Maybe[B]): A ==>> B =
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
              case Just(y) => Bin(kx, y, l, r)
              case Empty() => glue(l, r)
            }
        }
    }

  def deleteAt(i: Int): A ==>> B =
    updateAt(i, (A, B) => Maybe.empty)

  @tailrec
  final def findMin: Maybe[(A, B)] =
    this match {
      case Bin(kx, x, Tip(), _) =>
        just((kx, x))
      case Bin(_, _, l, _) =>
        l.findMin
      case Tip() =>
        Maybe.empty
    }

  @tailrec
  final def findMax: Maybe[(A, B)] =
    this match {
      case Bin(kx, x, _, Tip()) =>
        just((kx, x))
      case Bin(_, _, _, r) =>
        r.findMax
      case Tip() =>
        Maybe.empty
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

  def updateMin(f: B => Maybe[B]): A ==>> B =
    updateMinWithKey((_: A, b) => f(b))

  def updateMinWithKey(f: (A, B) => Maybe[B]): A ==>> B =
    this match {
      case Bin(kx, x, Tip(), r) =>
        f(kx, x) match {
          case Empty() =>
            r
          case Just(s) =>
            Bin(kx, s, Tip(), r)
        }
      case Bin(kx, x, l, r) =>
        balanceR(kx, x, l.updateMinWithKey(f), r)
      case Tip() =>
        empty
    }

  def updateMax(f: B => Maybe[B]): A ==>> B =
    updateMaxWithKey((_: A, b) => f(b))

  def updateMaxWithKey(f: (A, B) => Maybe[B]): A ==>> B =
    this match {
      case Bin(kx, x, l, Tip()) =>
        f(kx, x) match {
          case Empty() =>
            l
          case Just(s) =>
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
    alter(k, old => just(old.map(bsg.append(_, v)).getOrElse(v)))

  def minViewWithKey: Maybe[((A, B), A ==>> B)] =
    this match {
      case Tip() =>
        Maybe.empty
      case x @ Bin(_, _, _, _) =>
        just(deleteFindMin(x))
    }

  def maxViewWithKey: Maybe[((A, B), A ==>> B)] =
    this match {
      case Tip() =>
        Maybe.empty
      case x @ Bin(_, _, _, _) =>
        just(deleteFindMax(x))
    }

  def minView: Maybe[(B, A ==>> B)] =
    this match {
      case Tip() =>
        Maybe.empty
      case x @ Bin(_, _, _, _) =>
        val r = deleteFindMin(x)
        just((r._1._2, r._2))
    }

  def maxView: Maybe[(B, A ==>> B)] =
    this match {
      case Tip() =>
        Maybe.empty
      case x @ Bin(_, _, _, _) =>
        val r = deleteFindMax(x)
        just((r._1._2, r._2))
    }

  private def deleteFindMax(t: Bin[A, B]): ((A, B), A ==>> B) =
    t match {
      case Bin(k, x, l, Tip()) =>
        ((k,x), l)
      case Bin(k, x, l, r @ Bin(_, _, _, _)) =>
        val (km, r2) = deleteFindMax(r)
        (km, balanceL(k, x, l, r2))
    }

  private def deleteFindMin(t: Bin[A, B]): ((A, B), A ==>> B) =
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

  def traverseWithKey[F[_], C](f: (A, B) => F[C])(implicit G: Applicative[F]): F[A ==>> C] =
    this match {
      case Tip() =>
        G.point(Tip())
      case Bin(kx, x, Tip(), Tip()) =>
        G.apply(f(kx, x)) { x2 =>
          Bin(kx, x2, Tip(), Tip())
        }
      case Bin(kx, x, l, r) =>
        G.apply3(l.traverseWithKey(f), f(kx, x), r.traverseWithKey(f)) {
          (l2, x2, r2) => Bin(kx, x2, l2, r2)
        }
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

  def foldMapWithKey[C](f: (A, B) => C)(implicit F: Monoid[C]): C =
    this match {
      case Tip() =>
        F.zero
      case Bin(k, x, Tip(), Tip()) =>
        f(k, x)
      case Bin(k, x, l, r) =>
        F.append(l.foldMapWithKey(f), F.append(f(k, x), r.foldMapWithKey(f)))
    }

  /* Unions */
  def union(other: A ==>> B)(implicit k: Order[A]): A ==>> B = {
    def hedgeUnion(blo: Maybe[A], bhi: Maybe[A], m1: A ==>> B, m2: A ==>> B): A ==>> B =
      (m1, m2) match {
        case (t1, Tip()) =>
          t1
        case (Tip(), Bin(kx, x, l, r)) =>
          link(kx, x, l filterGt blo, r filterLt bhi)
        case (t1, Bin(kx, x, Tip(), Tip())) =>
          insertR(kx, x, t1)
        case (Bin(kx, x, l, r), t2) =>
          val bmi = just(kx)
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
      case (t1, t2)    => hedgeUnion(Maybe.empty, Maybe.empty, t1, t2)
    }
  }

  def unionWith(other: A ==>> B)(f: (B, B) => B)(implicit o: Order[A]): A ==>> B =
    unionWithKey(other)((_, b, c) => f(b, c))

  def unionWithKey(other: A ==>> B)(f: (A, B, B) => B)(implicit o: Order[A]): A ==>> B =
    mergeWithKey(this, other)((a, b, c) => just(f(a, b, c)))(x => x, x => x)

  // Difference functions
  def \\[C](other: A ==>> C)(implicit o: Order[A]): A ==>> B =
    difference(other)

  def difference[C](other: A ==>> C)(implicit o: Order[A]): A ==>> B = {
    def hedgeDiff(blo: Maybe[A], bhi: Maybe[A], a: A ==>> B, b: A ==>> C): A ==>> B =
      (a, b) match {
        case (Tip(), _) =>
          empty
        case (Bin(kx, x, l, r), Tip()) =>
          link(kx, x, l filterGt blo, r filterLt bhi)
        case (t, Bin(kx, _, l, r)) =>
          val bmi = just(kx)
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
        hedgeDiff(Maybe.empty, Maybe.empty, t1, t2)
    }
  }

  def differenceWith[C](other: A ==>> C)(f: (B, C) => Maybe[B])(implicit o: Order[A]): A ==>> B =
    differenceWithKey(other)((_, b, c) => f(b, c))

  def differenceWithKey[C](other: A ==>> C)(f: (A, B, C) => Maybe[B])(implicit o: Order[A]): A ==>> B =
    mergeWithKey(this, other)(f)(x => x, _ => empty)

  // Intersections
  def intersection[C](other: A ==>> C)(implicit o: Order[A]): A ==>> B = {
    def hedgeInt(blo: Maybe[A], bhi: Maybe[A], a: A ==>> B, b: A ==>> C): A ==>> B =
      (a, b) match {
        case (_, Tip()) =>
          empty
        case (Tip(), _) =>
          empty
        case (Bin(kx, x, l, r), t2) =>
          val bmi = just(kx)
          val l2 = hedgeInt(blo, bmi, l, ==>>.trim(blo, bmi, t2))
          val r2 = hedgeInt(bmi, bhi, r, ==>>.trim(bmi, bhi, t2))

          if (t2 member kx) link(kx, x, l2, r2)
          else l2 merge r2
      }

    (this, other) match {
      case (Tip(), _) => empty
      case (_, Tip()) => empty
      case (t1, t2)   => hedgeInt(Maybe.empty, Maybe.empty, t1, t2)
    }
  }

  def intersectionWith[C, D](other: A ==>> C)(f: (B, C) => D)(implicit o: Order[A]): A ==>> D =
    intersectionWithKey(other)((_, x, y) => f(x, y))

  def intersectionWithKey[C, D](other: A ==>> C)(f: (A, B, C) => D)(implicit o: Order[A]): A ==>> D =
    mergeWithKey(this, other)((a, b, c) => just(f(a, b, c)))(_ => empty, _ => empty)

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
          case Empty() =>
            false
          case Just(y) =>
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

  def mapMaybe[C](f: B => Maybe[C])(implicit o: Order[A]): A ==>> C =
    mapMaybeWithKey((_, x) => f(x))

  def mapMaybeWithKey[C](f: (A, B) => Maybe[C])(implicit o: Order[A]): A ==>> C =
    this match {
      case Tip() =>
        empty
      case Bin(kx, x, l, r) =>
        f(kx, x).cata(y =>
          link(kx, y, l.mapMaybeWithKey(f), r.mapMaybeWithKey(f)),
          l.mapMaybeWithKey(f).merge(r.mapMaybeWithKey(f))
        )
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

  def splitLookup(k: A)(implicit o: Order[A]): (A ==>> B, Maybe[B], A ==>> B) =
    this match {
      case Tip() =>
        (empty, Maybe.empty, empty)
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            val (lt, z, gt) = l splitLookup k
            (lt, z, link(kx, x, gt, r))
          case GT =>
            val (lt, z, gt) = r splitLookup k
            (link(kx, x, l, lt), z, gt)
          case EQ =>
            (l, just(x), r)
        }
    }

  def splitLookupWithKey(k: A)(implicit o: Order[A]): (A ==>> B, Maybe[(A, B)], A ==>> B) =
    this match {
      case Tip() =>
        (empty, Maybe.empty, empty)
      case Bin(kx, x, l, r) =>
        o.order(k, kx) match {
          case LT =>
            val (lt, z, gt) = l splitLookupWithKey k
            (lt, z, link(kx, x, gt, r))
          case GT =>
            val (lt, z, gt) = r splitLookupWithKey k
            (link(kx, x, l, lt), z, gt)
          case EQ =>
            (l, just((kx, x)), r)
        }
    }

  def splitRoot: IList[A ==>> B] =
    this match {
      case Tip()           => IList.empty[A ==>> B]
      case Bin(k, x, l, r) => IList(l, singleton(k, x), r)
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

  @tailrec
  private final def trim(lo: A => Ordering, hi: A => Ordering): A ==>> B =
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

  @tailrec
  private final def trimLookupLo(lo: A, cmphi: A => Ordering)(implicit o: Order[A]): (Maybe[(A, B)], A ==>> B) =
    this match {
      case Tip() =>
        (Maybe.empty, empty)
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
            (just((kx, x)), r.trim(a => o.order(lo, a), cmphi))
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
  private def filterGt(a: Maybe[A])(implicit o: Order[A]): A ==>> B = {
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

    a.cata(filter(_, this), this)
  }

  private def filterLt(a: Maybe[A])(implicit o: Order[A]): A ==>> B = {
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

    a.cata(filter(_, this), this)
  }

}

sealed abstract class MapInstances2 {
  implicit def mapBand[A, B](implicit A: Order[A], B: Band[B]): Band[A ==>> B] = new Band[A ==>> B] {
    def append(a: A ==>> B, b: => A ==>> B): A ==>> B =
      (a unionWith b)(B.append(_, _))
  }
}

sealed abstract class MapInstances1 extends MapInstances2 {
  implicit def mapLattice[A, B](implicit A: Order[A], B: SemiLattice[B]): SemiLattice[A ==>> B] = new SemiLattice[A ==>> B] {
    def append(a: A ==>> B, b: => A ==>> B): A ==>> B =
      (a unionWith b)(B.append(_, _))
  }
}

sealed abstract class MapInstances0 extends MapInstances1 {

  implicit def scalazMapInstance[S: Order]: Bind[S ==>> *] with Align[S ==>> *] with Zip[S ==>> *] =
    new Bind[S ==>> *] with Align[S ==>> *] with Zip[S ==>> *] {
      override def map[A, B](fa: S ==>> A)(f: A => B) =
        fa map f

      def bind[A, B](fa: S ==>> A)(f: A => (S ==>> B)) =
        fa.mapMaybeWithKey((k, v) => f(v).lookup(k))

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

      override def zipWith[A, B, C](a: => (S ==>> A), b: => (S ==>> B))(f: (A, B) => C)(implicit F: Functor[S ==>> *]) = {
        val a0 = a
        if(a0.isEmpty) ==>>.empty
        else a0.intersectionWith(b)(f)
      }
    }
}

sealed abstract class MapInstances extends MapInstances0 {
  import ==>>._

  import std.tuple._
  import std.list._

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

  implicit def mapCovariant[S]: Traverse[S ==>> *] =
    new Traverse[S ==>> *] {
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
        fa.elemAt(i).map(_._2).toOption

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
  private[scalaz] sealed abstract case class Tip[A, B] private () extends (A ==>> B) {
    val size = 0
  }
  private[scalaz] object Tip {
    private[this] val value: Tip[Nothing, Nothing] = new Tip[Nothing, Nothing]{}
    def apply[A, B](): A ==>> B = value.asInstanceOf[A ==>> B]
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

  final def fromIList[A: Order, B](l: IList[(A, B)]): A ==>> B =
    l.foldLeft(empty[A, B]) { (t, x) => t.insert(x._1, x._2) }

  final def fromListWith[A: Order, B](l: List[(A, B)])(f: (B, B) => B): A ==>> B =
    fromListWithKey(l)((_, x, y) => f(x, y))

  final def fromIListWith[A: Order, B](l: IList[(A, B)])(f: (B, B) => B): A ==>> B =
    fromIListWithKey(l)((_, x, y) => f(x, y))

  final def fromListWithKey[A: Order, B](l: List[(A, B)])(f: (A, B, B) => B): A ==>> B =
    l.foldLeft(empty[A, B])((a, c) => a.insertWithKey(f, c._1, c._2))

  final def fromIListWithKey[A: Order, B](l: IList[(A, B)])(f: (A, B, B) => B): A ==>> B =
    l.foldLeft(empty[A, B])((a, c) => a.insertWithKey(f, c._1, c._2))

  /* TODO: Ordered lists
    , fromAscList
    , fromAscListWith
    , fromAscListWithKey
    , fromDistinctAscList
  */

  /* Foldable operations */
  final def fromFoldable[F[_]: Foldable, A: Order, B](fa: F[(A, B)]): A ==>> B =
    Foldable[F].foldLeft(fa, empty[A, B]) { (t, x) => t.insert(x._1, x._2) }

  final def fromFoldableWith[F[_]: Foldable, A: Order, B](fa: F[(A, B)])(f: (B, B) => B): A ==>> B =
    fromFoldableWithKey(fa)((_, x, y) => f(x, y))

  final def fromFoldableWithKey[F[_]: Foldable, A: Order, B](fa: F[(A, B)])(f: (A, B, B) => B): A ==>> B =
    Foldable[F].foldLeft(fa, empty[A, B])((a, c) => a.insertWithKey(f, c._1, c._2))

  final def fromSet[A: Order, B](s: ISet[A])(f: A => B): A ==>> B =
    s match {
      case ISet.Tip() =>
        empty
      case ISet.Bin(x, l, r) =>
        Bin(x, f(x), fromSet(l)(f), fromSet(r)(f))
    }

  final def unions[A: Order, B](xs: IList[A ==>> B]): A ==>> B =
    xs.foldLeft(empty[A, B])((a, c) => a.union(c))

  final def unionsWith[A: Order, B](f: (B, B) => B)(xs: IList[A ==>> B]): A ==>> B =
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

  private[scalaz] def trim[A, B](lo: Maybe[A], hi: Maybe[A], t: A ==>> B)(implicit o: Order[A]): A ==>> B =
    (lo, hi) match {
      case (Maybe.Empty(), Maybe.Empty()) =>
        t
      case (Just(lk), Maybe.Empty()) =>
        @tailrec
        def greater(lo: A, m: A ==>> B): A ==>> B =
          m match {
            case Bin(kx, _, _, r) if o.lessThanOrEqual(kx, lo) =>
              greater(lo, r)
            case _ =>
              m
          }
        greater(lk, t)
      case (Maybe.Empty(), Just(hk)) =>
        @tailrec
        def lesser(hi: A, m: A ==>> B): A ==>> B =
          m match {
            case Bin(kx, _, l, _) if o.greaterThanOrEqual(kx, hi) =>
              lesser(hi, l)
            case _ =>
              m
          }
        lesser(hk, t)
      case (Just(lk), Just(hk)) =>
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

  private[scalaz] def trimLookupLo[A, B](lk: A, hkMaybe: Maybe[A], t: A ==>> B)(implicit o: Order[A]): (Maybe[B], A ==>> B) =
    hkMaybe match {
      case Empty() =>
        @tailrec
        def greater(lo: A, m: A ==>> B): (Maybe[B], A ==>> B) =
          m match {
            case Tip() =>
              (Maybe.empty, Tip())
            case Bin(kx, x, l, r) =>
              o.order(lo, kx) match {
                case LT =>
                  (l.lookup(lo), m)
                case EQ =>
                  (just(x), r)
                case GT =>
                  greater(lo, r)
              }
          }
        greater(lk, t)
      case Just(hk) =>
        @tailrec
        def middle(lo: A, hi: A, m: A ==>> B): (Maybe[B], A ==>> B) =
          m match {
            case Tip() =>
              (Maybe.empty, Tip())
            case Bin(kx, x, l, r) =>
              o.order(lo, kx) match {
                case LT if o.order(kx, hi) == LT =>
                  (l.lookup(lo), m)
                case LT =>
                  middle(lo, hi, l)
                case EQ =>
                  (just(x), lesser(hi, r))
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

 /*
  * The documentation below is partially copied from haskell/containers Map.
  * Please refer to the full documentation if necessary.
  * @see [[https://github.com/haskell/containers/blob/v0.5.7.1/Data/Map/Base.hs#L1402-L1440]]
  *
  -- | /O(n+m)/. A high-performance universal combining function. This function
  -- is used to define 'unionWith', 'unionWithKey', 'differenceWith',
  -- 'differenceWithKey', 'intersectionWith', 'intersectionWithKey' and can be
  -- used to define other custom combine functions.

  -- When calling mergeWithKey(a, b)(f)(g1, g2), a function combining two
  -- 'Map's is created, such that
  --
  -- * if a key is present in both maps, it is passed with both corresponding
  --   values to the combine @f@ function. Depending on the result, the key is either
  --   present in the result with specified value, or is left out;
  --
  -- * a nonempty subtree present only in the first map is passed to @g1@ and
  --   the output is added to the result;
  --
  -- * a nonempty subtree present only in the second map is passed to @g2@ and
  --   the output is added to the result.
  *
  *  @param  a   first map
  *  @param  b   second map
  *  @param  f   combine function used to define `XxxWithKey`
  *  @param  g1  function applied to a nonempty subtree present only in the first map a
  *  @param  g2  function applied to a nonempty subtree present only in the second map b
  */
  def mergeWithKey[A: Order, B, C, D](a: A ==>> B, b: A ==>> C)
                                     (f: (A, B, C) => Maybe[D])
                                     (g1: (A ==>> B) => (A ==>> D), g2: (A ==>> C) => (A ==>> D))
                                     (implicit o: Order[A]): A ==>> D = {
    def hedgeMerge(blo: Maybe[A], bhi: Maybe[A], a: A ==>> B, b: A ==>> C): A ==>> D = {
      (a, b) match {
        case (t1, Tip()) => g1(t1)
        case (Tip(), Bin(kx, x, l, r)) =>
          val t2 = link(kx, x, l.filterGt(blo)(o), r.filterLt(bhi)(o))
          g2(t2)
        case (Bin(kx, x, l, r), t2) =>
          val bmi = just(kx)
          val l2 = hedgeMerge(blo, bmi, l, trim(blo, bmi, t2)(o))
          val (found, trim_t2) = trimLookupLo(kx, bhi, t2)(o)
          val r2 = hedgeMerge(bmi, bhi, r, trim_t2)
          found match {
            case Maybe.Empty() =>
              g1(singleton(kx, x)) match {
                case Tip() =>
                  l2 merge r2
                case Bin(_, x2, Tip(), Tip()) =>
                  link(kx, x2, l2, r2)
                case _ =>
                  sys.error("mergeWithKey: Given function g1 does not fulfil required conditions (see documentation)")
              }
            case Just(x2) =>
              f(kx, x, x2) match {
                case Maybe.Empty() =>
                  l2 merge r2
                case Just(xx) =>
                  link(kx, xx, l2, r2)
              }
          }
      }
    }

    (a, b) match {
      case (Tip(), t2) => g2(t2)
      case (t1, Tip()) => g1(t1)
      case (t1, t2)    => hedgeMerge(Maybe.empty, Maybe.empty, t1, t2)
    }
  }
}
