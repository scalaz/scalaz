package scalaz

import std.tuple._
import scala.annotation.tailrec

/**An efficient, asymptotically optimal, implementation of priority queues
 * extended with support for efficient size.
 *
 * The implementation of 'Heap' is based on bootstrapped skew binomial heaps
 * as described by:
 * G. Brodal and C. Okasaki , "Optimal Purely Functional Priority Queues",
 *    Journal of Functional Programming 6:839-857 (1996),
 *
 * Based on the heaps Haskell library by Edward Kmett
 */
sealed abstract class Heap[A] {

  import Heap._
  import Heap.impl._
  import Cofree._

  type CTree[A] = Cofree[IList, A]

  def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, CTree[Ranked[A]]) => B): B

  /**Is the heap empty? O(1)*/
  def isEmpty = fold(true, (_, _, _) => false)

  /**Is the heap populated? O(1)*/
  final def nonEmpty = !isEmpty

  /**The number of elements in the heap. O(1)*/
  def size = fold(0, (s, _, _) => s)

  /**Insert a new value into the heap. O(1)*/
  def insert(a: A)(implicit o: Order[A]) = insertWith(o.lessThanOrEqual, a)

  /** Alias for insert */
  final def +(a: A)(implicit o: Order[A]) = this insert a

  def insertAll(as: TraversableOnce[A])(implicit o: Order[A]): Heap[A] =
    (this /: as)((h,a) => h insert a)

  def insertAllF[F[_]](as: F[A])(implicit F: Foldable[F], o: Order[A]): Heap[A] =
    F.foldLeft(as, this)((h,a) => h insert a)

  /**Meld the values from two heaps into one heap. O(1)*/
  def union(as: Heap[A]) = (this, as) match {
    case (Empty(), q)                         => q
    case (q, Empty())                         => q
    case (Heap(s1, leq, t1 @ Cofree(Ranked(r1, x1), f1)),
    Heap(s2, _, t2 @ Cofree(Ranked(r2, x2), f2))) =>
      if (leq(x1, x2)) {
        Heap(s1 + s2, leq, Cofree(Ranked(0, x1), skewInsert(leq, t2, f1)))
      } else {
        Heap(s1 + s2, leq, Cofree(Ranked(0, x2), skewInsert(leq, t1, f2)))
      }
  }

  /**Split the heap into the minimum element and the remainder. O(log n)*/
  def uncons: Option[(A, Heap[A])] =
    fold(None, (_, _, t) => Some((t.head.value, deleteMin)))

  /**Get the minimum key on the (nonempty) heap. O(1) */
  def minimum: A = fold(sys.error("Heap.minimum: empty heap"), (_, _, t) => t.head.value)

  /**Get the minimum key on the (nonempty) heap. O(1) */
  def minimumO: Option[A] = fold(None, (_, _, t) => Some(t.head.value))

  /**Delete the minimum key from the heap and return the resulting heap. O(log n) */
  def deleteMin: Heap[A] = {
    fold(Empty[A], (s, leq, t) => t match {
      case Cofree(_, INil()) => Empty[A]
      case Cofree(_, f0)       => {
        val (Cofree(Ranked(r, x), cf), ts2) = getMin(leq, f0)
        val (zs, ts1, f1) = splitForest(r, empty[A](), empty[A](), cf)
        val f2 = skewMeld(leq, skewMeld(leq, ts1, ts2), f1)
        val f3 = zs.foldRight(f2)(skewInsert(leq, _, _))
        Heap(s - 1, leq, Cofree(Ranked(0, x), f3))
      }
    })
  }

  def adjustMin(f: A => A): Heap[A] = this match {
    case Heap(s, leq, Cofree(Ranked(r, x), xs)) =>
      Heap(s, leq, heapify(leq)( Cofree(Ranked(r, f(x)), xs)))
  }

  def toUnsortedStream: Stream[A] = toUnsortedList.toStream 

  def toUnsortedList: IList[A] = fold(INil[A](), (_, _, t) => Foldable[CTree].foldRight(t, (INil(): IList[A]))((a,b) => a.value :: b) ) 

  def toList: IList[A] = toListHeap(this, INil())
  
  def toStream: Stream[A] = toList.toStream 

  /**Map a function over the heap, returning a new heap ordered appropriately. O(n)*/
  def map[B: Order](f: A => B): Heap[B] = fold(Empty[B], (_, _, t) => Foldable[CTree].foldMap(t)(x => singleton(f(x.value))))

  def forall(f: A => Boolean) = toStream.forall(f)

  def exists(f: A => Boolean) = toStream.exists(f)

  def foreach(f: A => Unit) = toStream.foreach(f)

  /**Filter the heap, retaining only values that satisfy the predicate. O(n)*/
  def filter(p: A => Boolean): Heap[A] = 
    fold(Empty[A], (_, leq, t) => Foldable[CTree].foldMap(t)(x => if (p(x.value)) singletonWith(leq, x.value) else Empty[A]))

  /**Partition the heap according to a predicate. The first heap contains all elements that
   * satisfy the predicate. The second contains all elements that fail the predicate. O(n)*/
  def partition(p: A => Boolean): (Heap[A], Heap[A]) =
    fold((Empty[A], Empty[A]), (_, leq, t) => Foldable[CTree].foldMap(t)(x =>
      if (p(x.value)) (singletonWith(leq, x.value), Empty[A])
      else
        (Empty[A], singletonWith(leq, x.value))))

  /**Partition the heap of the elements that are less than, equal to, and greater than a given value. O(n)*/
  def split(a: A): (Heap[A], Heap[A], Heap[A]) = {
    fold((Empty[A], Empty[A], Empty[A]), (s, leq, t) => {
      def f(x: A) = if (leq(x, a)) if (leq(a, x)) (Empty[A], singletonWith(leq, x), Empty[A])
      else
        (singletonWith(leq, x), Empty[A], Empty[A])
      else
        (Empty[A], Empty[A], singletonWith(leq, x))
      Foldable[CTree].foldMap(t)(x => f(x.value))
    })
  }

  /**Return a heap consisting of the least n elements of this heap. O(n log n) */
  def take(n: Int) = withList(_.take(n))

  /**Return a heap consisting of all the members of this heap except for the least n. O(n log n) */
  def drop(n: Int) = withList(_.drop(n))

  /**Split into two heaps, the first containing the n least elements, the second containing the n
   * greatest elements. O(n log n) */
  def splitAt(n: Int) = splitWithList(_.splitAt(n))

  /**Returns a tuple where the first element is a heap consisting of the longest prefix of least elements
   * in this heap that do not satisfy the given predicate, and the second element is the remainder
   * of the elements. O(n log n) */
  def break(p: A => Boolean): (Heap[A], Heap[A]) =
    span(x => !p(x))

  /**Returns a tuple where the first element is a heap consisting of the longest prefix of least elements
   * in this heap that satisfy the given predicate and the second element is the remainder of the elements.
   * O(n log n)*/
  def span(p: A => Boolean): (Heap[A], Heap[A]) =
    splitWithList(_.span(p))

  /**Returns a heap consisting of the longest prefix of least elements of this heap that satisfy the predicate.
   * O(n log n) */
  def takeWhile(p: A => Boolean) =
    withList(_.takeWhile(p))

  /**Returns a heap consisting of the longest prefix of least elements of this heap that do not
   * satisfy the predicate. O(n log n) */
  def dropWhile(p: A => Boolean) =
    withList(_.dropWhile(p))

/**Remove duplicate entries from the heap. O(n log n)*/
  def nub: Heap[A] = fold(Empty[A], (_, leq, t) => {
    val x = t.head.value
    val xs = deleteMin
    val zs = xs.dropWhile(leq(_, x))
    zs.nub.insertWith(leq, x)
  })

  /**Construct heaps from each element in this heap and union them together into a new heap. O(n)*/
  def flatMap[B: Order](f: A => Heap[B]): Heap[B] =
    fold(Empty[B], (_, _, t) => Foldable[CTree].foldMap(t)(x => f(x.value)))

  /**Traverse the elements of the heap in sorted order and produce a new heap with applicative effects.
   * O(n log n)*/
  def traverse[F[_] : Applicative, B: Order](f: A => F[B]): F[Heap[B]] = {
    val F = Applicative[F]
    import std.stream._
    F.map(F.traverse(toList)(f))(fromCodata[IList, B])
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = Foldable[IList].foldRight(toList, z)(f)

  private def withList(f: IList[A] => IList[A]) = {
    import std.list._
    fold(Empty[A], (_, leq, _) => fromDataWith(leq, f(toList)))
  }

  private[scalaz] def insertWith(f: (A, A) => Boolean, x: A) =
    fold(singletonWith(f, x), (s, _, t) => {
      val y = t.head.value
      if (f(x, y)) {
        Heap(s + 1, f, Cofree(Ranked(0, x), IList(t)))
      } else {
        val skewed = skewInsert(f, Cofree(Ranked(0, x), IList[CTree[Ranked[A]]]()), t.tail) 
        Heap(s + 1, f, Cofree(Ranked(0, y),
          skewed))
      }
    })

  private def splitWithList(f: IList[A] => (IList[A], IList[A])) = {
    import std.list._

    fold((Empty[A], Empty[A]), (_, leq, _) => {
      val g = (x: IList[A]) => fromDataWith(leq, x)
      val x = f(toList)
      (g(x._1), g(x._2))
    })
  }

  override def toString = "<heap>"
}

case class Ranked[A](rank: Int, value: A)

object Heap extends HeapInstances with HeapFunctions {
  import Cofree._
  type CTree[A] = Cofree[IList, A]


  @tailrec private def toListHeap[A](h: Heap[A], l: IList[A]): IList[A] = h.uncons match {
    case Some((a, h)) => 
      toListHeap(h, a :: l)
    case None => l.reverse
  }
      
  def hnode[A](root: => A, forest: => IList[CTree[A]]): CTree[A] = Cofree(root, forest)
  
  def apply[A](sz: Int, leq: (A, A) => Boolean, t: CTree[Ranked[A]]): Heap[A] = new Heap[A] {
    def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, CTree[Ranked[A]]) => B) =
      nonempty(sz, leq, t)
  }

  def unapply[A](h: Heap[A]): Option[(Int, (A, A) => Boolean, CTree[Ranked[A]])] =
    h.fold(None, (sz, leq, t) => Some((sz, leq, t)))

  private[scalaz] object impl {
    import Tree._

    def rightZ[A]: ForestZipper[A] => ForestZipper[A] = {
      case (path, a @ ICons(x, xs)) => (x :: path, xs)
      case t @ _ => (t._1, t._2) //stream checks were not exhaustive due to stream not being sealed, so we need to handle this case now
    }

    def adjustZ[A](f: CTree[Ranked[A]] => CTree[Ranked[A]]):
    ForestZipper[A] => ForestZipper[A] = {
      case (path, ICons(x, xs)) => (path, f(x) :: xs)
      case z                => z
    }

    def rezip[A]: ForestZipper[A] => Forest[A] = {
      case (INil(), xs)   => xs
      case (ICons(x, path), xs) => rezip((path, x :: xs))
    }

    def rootZ[A]: ForestZipper[A] => A = {
      case (_, ICons(x, _)) => x.head.value
      case _            => sys.error("Heap.rootZ: empty zipper")
    }

    def zipper[A](xs: Forest[A]): ForestZipper[A] = (INil(), xs)

    def emptyZ[A]: ForestZipper[A] = (INil(), INil())

    def minZ[A](f: (A, A) => Boolean): Forest[A] => ForestZipper[A] = {
      case INil() => emptyZ
      case xs       => {
        val z = zipper(xs)
        minZp(f)(z, z)
      }
    }

    def minZp[A](leq: (A, A) => Boolean):
    (ForestZipper[A], ForestZipper[A]) => ForestZipper[A] = {
      case (lo, (_, INil())) => lo
      case (lo, z)             => minZp(leq)(if (leq(rootZ(lo), rootZ(z))) lo else z, rightZ(z))
    }
    
    def heapify[A](leq: (A, A) => Boolean): CTree[Ranked[A]] => CTree[Ranked[A]] = {
      case n @ Cofree(_, INil())      => n
      case n@ Cofree(Ranked(r, a), as) => {
        val (left, ICons(Cofree(Ranked(rp, ap), asp), right)) = minZ(leq)(as) 
        if (leq(a, ap)) {
          n
        } else {
          Cofree(Ranked(r, ap), rezip(left, heapify(leq)(Cofree(Ranked(rp, a), asp)) :: right))
        }
      }
    }

    def singletonWith[A](f: (A, A) => Boolean, a: A) =
      Heap(1, f, Cofree(Ranked(0, a), IList[CTree[Ranked[A]]]()))


    def rank[A](t: CTree[Ranked[A]]) = t.head.rank

    def skewLink[A](f: (A, A) => Boolean,
                    t0: CTree[Ranked[A]],
                    t1: CTree[Ranked[A]],
                    t2: CTree[Ranked[A]]): CTree[Ranked[A]] = (t0, t1, t2) match {
      case (Cofree(Ranked(r0, x0), cf0), Cofree(Ranked(r1, x1), cf1), Cofree(Ranked(r2, x2), cf2)) =>
        if (f(x1, x0) && f(x1, x2)) Cofree(Ranked(r1 + 1, x1), t0 :: t2 :: cf1)
        else
        if (f(x2, x0) && f(x2, x1)) Cofree(Ranked(r2 + 1, x2), t0 :: t1 :: cf2)
        else
          Cofree(Ranked(r1 + 1, x0), t1 :: t2 :: cf0)
    }

    def link[A](f: (A, A) => Boolean):
    (CTree[Ranked[A]],CTree[Ranked[A]]) => CTree[Ranked[A]] = {
      case (t1 @ Cofree(Ranked(r1, x1), cf1), t2 @ Cofree(Ranked(r2, x2), cf2)) =>
        if (f(x1, x2)) Cofree(Ranked(r1 + 1, x1), t2 :: cf1)
        else
          Cofree(Ranked(r2 + 1, x2), t1 :: cf2)
    }
    
    def skewInsert[A](f: (A, A) => Boolean, t: CTree[Ranked[A]], ts: Forest[A]): Forest[A] = 
      ts match {
        case ICons(t1, ICons(t2, rest)) =>
          if (rank(t1) == rank(t2)) {
            skewLink(f, t, t1, t2) :: rest
          } else (t :: ts)
        case _                  => 
          t :: ts
      }

    def getMin[A](f: (A, A) => Boolean, trees: Forest[A]): (CTree[Ranked[A]], Forest[A]) =
      trees match {
       case ICons(Cofree(h, t),INil()) => 
        (Cofree(h, t), INil()) 
        case ICons(t, ts)  => {
          val (tp, tsp) = getMin(f, ts)
          if (f(t.head.value, tp.head.value)) (t, ts) else (tp, t :: tsp)
        }
      }

    def splitForest[A]:
    (Int, Forest[A], Forest[A], Forest[A]) => (Forest[A], Forest[A], Forest[A]) = {
      case (0, zs, ts, f)                  => (zs, ts, f)
      case (1, zs, ts, ICons(tt, INil()))          => 
        (zs, tt :: ts, INil())
      case (1, zs, ts, ICons(t1, ICons(t2, f))) =>
        if (rank(t2) == 0) 
          (t1 :: zs, t2 :: ts, f)
        else
          (zs, t1 :: ts, t2 :: f)
      case (r, zs, ts, ICons(t1, ICons(t2, cf))) =>
        if (rank(t1) == rank(t2))
          (zs, t1 :: t2 :: ts, cf)
        else
          if (rank(t1) == 0) 
            splitForest(r - 1, t1 :: zs, t2 :: ts, cf)
          else
            splitForest(r - 1, zs, t1 :: ts, t2 :: cf)
        case (_, _, _, _)                    => sys.error("Heap.splitForest: invalid arguments")
    }

    def skewMeld[A](f: (A, A) => Boolean, ts: Forest[A], tsp: Forest[A]) =
      unionUniq(f)(uniqify(f)(ts), uniqify(f)(tsp))

    def ins[A](f: (A, A) => Boolean, t: CTree[Ranked[A]]): Forest[A] => Forest[A] = {
      case INil()    => IList(t)
      case ICons(tp, ts) => if (rank(t) < rank(tp)) t :: tp :: ts
      else
        ins(f, link(f)(t, tp))(ts)
    }

    def uniqify[A](f: (A, A) => Boolean): Forest[A] => Forest[A] = {
      case INil()   => INil()
      case ICons(t,ts) => ins(f, t)(ts)
    }

    def unionUniq[A](f: (A, A) => Boolean): (Forest[A], Forest[A]) => Forest[A] = {
      case (INil(), ts)                         => ts
      case (ts, INil())                         => ts
      case (tts1@ ICons(t1, ts1), tts2 @ ICons(t2, ts2)) =>
        import std.anyVal._
        Order[Int].order(rank(t1), rank(t2)) match {
          case Ordering.LT => t1 :: unionUniq(f)(ts1, tts2)
          case Ordering.EQ => ins(f, link(f)(t1, t2))(unionUniq(f)(ts1, ts2))
          case Ordering.GT => t2 :: unionUniq(f)(tts1, ts2)
        }
    }
  }
  
}


sealed abstract class HeapInstances {
  
  import Heap._

  implicit val ctreeInstance = new Foldable[CTree] {
    
    def foldMap[A,B](ct: CTree[A])(f: A => B)(implicit M: Monoid[B]): B = {
      val root = f(ct.head)
      val tail: B = Foldable[IList].foldMap(ct.tail)(cf => {
        foldMap(cf)(f)(M)
      })
      M.append(root, tail)
    } 
    
    def foldRight[A, B](ct: CTree[A], z: => B)(f: (A, =>B) => B): B = {
      Foldable[IList].foldRight(Cofree[IList, A](ct.head, INil()) :: ct.tail, z)( (a,b) => {
          Foldable[CTree].foldRight(a, b)(f) 
      }) 
    }
  }
  
  implicit val heapInstance = new Foldable[Heap] with Foldable.FromFoldr[Heap] {
    def foldRight[A, B](fa: Heap[A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
  }

  implicit def heapMonoid[A]: Monoid[Heap[A]] = new Monoid[Heap[A]] {
    def append(f1: Heap[A], f2: => Heap[A]) = f1 union f2
    def zero = Heap.Empty.apply
  }


  import std.stream._

  implicit def heapEqual[A: Equal]: Equal[Heap[A]] = Equal.equalBy((_: Heap[A]).toList)
}


trait HeapFunctions {
  
  import Heap._
  
  type Forest[A] = IList[Cofree[IList, Ranked[A]]]
  
  type ForestZipper[A] = (Forest[A], Forest[A])

  def empty[A](): Forest[A] = INil[CTree[Ranked[A]]]()

  /**The empty heap */
  object Empty {
    def apply[A]: Heap[A] = new Heap[A] {
      def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, CTree[Ranked[A]]) => B): B = empty
    }

    def unapply[A](h: Heap[A]): Boolean = h.fold(true, (_, _, _) => false)
  }

  import Heap.impl._

  def fromData[F[_] : Foldable, A: Order](as: F[A]): Heap[A] =
    Foldable[F].foldLeft(as, Empty[A])((b, a) => b insert a)

  def fromCodata[F[_] : Foldable, A: Order](as: F[A]): Heap[A] =
    Foldable[F].foldr(as, Empty[A])(x => y => y insert x)

  def fromDataWith[F[_] : Foldable, A](f: (A, A) => Boolean, as: F[A]): Heap[A] =
    Foldable[F].foldLeft(as, Empty[A])((x, y) => x.insertWith(f, y))

  /**Heap sort */
  def sort[F[_] : Foldable, A: Order](xs: F[A]): IList[A] = fromData(xs).toList

  /**Heap sort */
  def sortWith[F[_] : Foldable, A](f: (A, A) => Boolean, xs: F[A]): IList[A] = fromDataWith(f, xs).toList

  /**A heap with one element. */
  def singleton[A: Order](a: A): Heap[A] = singletonWith[A](Order[A].lessThanOrEqual, a)

  /**Create a heap consisting of multiple copies of the same value. O(log n) */
  def replicate[A: Order](a: A, i: Int): Heap[A] = {
    def f(x: Heap[A], y: Int): Heap[A] =
      if (y % 2 == 0) f(x union x, y / 2)
      else
      if (y == 1) x
      else
        g(x union x, (y - 1) / 2, x)
    def g(x: Heap[A], y: Int, z: Heap[A]): Heap[A] =
      if (y % 2 == 0) g(x union x, y / 2, z)
      else
      if (y == 1) x union z
      else
        g(x union x, (y - 1) / 2, x union z)
    if (i < 0) sys.error("Heap.replicate: negative length")
    else
    if (i == 0) Empty[A]
    else
      f(singleton(a), i)
  }
}
