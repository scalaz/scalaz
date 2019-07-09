package scalaz

import std.tuple._
import EphemeralStream.{EStream, emptyEphemeralStream, ##::}


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
  import Tree._

  def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, Tree[Ranked[A]]) => B): B

  /**Is the heap empty? O(1)*/
  def isEmpty: Boolean = fold(true, (_, _, _) => false)

  /**Is the heap populated? O(1)*/
  final def nonEmpty: Boolean = !isEmpty

  /**The number of elements in the heap. O(1)*/
  def size: Int = fold(0, (s, _, _) => s)

  /**Insert a new value into the heap. O(1)*/
  def insert(a: A)(implicit o: Order[A]): Heap[A] = insertWith(o.lessThanOrEqual, a)

  /** Alias for insert */
  final def +(a: A)(implicit o: Order[A]): Heap[A] = this insert a

  def insertAll(as: TraversableOnce[A])(implicit o: Order[A]): Heap[A] =
    (this /: as)((h,a) => h insert a)

  def insertAllF[F[_]](as: F[A])(implicit F: Foldable[F], o: Order[A]): Heap[A] =
    F.foldLeft(as, this)((h,a) => h insert a)

  /**Meld the values from two heaps into one heap. O(1)*/
  def union(as: Heap[A]): Heap[A] = (this, as) match {
    case (Empty(), q)                         => q
    case (q, Empty())                         => q
    case (Heap(s1, leq, t1@Node(Ranked(r1, x1), f1)),
    Heap(s2, _, t2@Node(Ranked(r2, x2), f2))) =>
      if (leq(x1, x2))
        Heap(s1 + s2, leq, Node(Ranked(0, x1), skewInsert(leq, t2, f1)))
      else
        Heap(s1 + s2, leq, Node(Ranked(0, x2), skewInsert(leq, t1, f2)))
  }

  /**Split the heap into the minimum element and the remainder. O(log n)*/
  def uncons: Option[(A, Heap[A])] =
    fold(None, (_, _, t) => Some((t.rootLabel.value, deleteMin)))

  /**Get the minimum key on the (nonempty) heap. O(1) */
  def minimum: A = fold(sys.error("Heap.minimum: empty heap"), (_, _, t) => t.rootLabel.value)

  /**Get the minimum key on the (nonempty) heap. O(1) */
  def minimumO: Option[A] = fold(None, (_, _, t) => Some(t.rootLabel.value))

  /**Delete the minimum key from the heap and return the resulting heap. O(log n) */
  def deleteMin: Heap[A] = {
    fold(Empty[A], (s, leq, t) => t match {
      case Leaf(_) => Empty[A]
      case Node(_, f0)       => {
        val (Node(Ranked(r, x), cf), ts2) = getMin(leq, f0)
        val (zs, ts1, f1) : (Forest[A], Forest[A], Forest[A]) = splitForest(r, emptyEphemeralStream, emptyEphemeralStream, cf)
        val f2 = skewMeld(leq, skewMeld(leq, ts1, ts2), f1)
        val f3 = zs.foldRight(f2)(skewInsert(leq, _, _))
        Heap(s - 1, leq, Node(Ranked(0, x), f3))
      }
    })
  }

  def adjustMin(f: A => A): Heap[A] = this match {
    case Heap(s, leq, Node(Ranked(r, x), xs)) =>
      Heap(s, leq, heapify(leq)(Node(Ranked(r, f(x)), xs)))
  }

  def toUnsortedStream: EStream[A] = fold(emptyEphemeralStream, (_, _, t) => t.flatten.map(_.value))

  def toUnsortedList: List[A] = toUnsortedStream.toList

  def toUnsortedIList: IList[A] = IList.fromFoldable(toUnsortedStream)

  def toEphemeralStream: EStream[A] =
    EphemeralStream.unfold(this)(_.uncons)

  def toStream: Stream[A] = 
    Foldable[EphemeralStream].toStream(this.toEphemeralStream)

  def toList: List[A] = toStream.toList

  def toIList: IList[A] = toEphemeralStream.toIList

  /**Map a function over the heap, returning a new heap ordered appropriately. O(n)*/
  def map[B: Order](f: A => B): Heap[B] = fold(Empty[B], (_, _, t) => t.foldMap(x => singleton(f(x.value))))

  def forall(f: A => Boolean): Boolean = toStream.forall(f)

  def exists(f: A => Boolean): Boolean = toStream.exists(f)

  def foreach(f: A => Unit): Unit = toStream.foreach(f)

  /**Filter the heap, retaining only values that satisfy the predicate. O(n)*/
  def filter(p: A => Boolean): Heap[A] =
    fold(Empty[A], (_, leq, t) => t foldMap (x => if (p(x.value)) singletonWith(leq, x.value) else Empty[A]))

  /**Partition the heap according to a predicate. The first heap contains all elements that
   * satisfy the predicate. The second contains all elements that fail the predicate. O(n)*/
  def partition(p: A => Boolean): (Heap[A], Heap[A]) =
    fold((Empty[A], Empty[A]), (_, leq, t) => t.foldMap(x =>
      if (p(x.value)) (singletonWith(leq, x.value), Empty[A])
      else
        (Empty[A], singletonWith(leq, x.value))))

  /**Partition the heap of the elements that are less than, equal to, and greater than a given value. O(n)*/
  def split(a: A): (Heap[A], Heap[A], Heap[A]) = {
    fold((Empty[A], Empty[A], Empty[A]), (s, leq, t) => {
      def f(x: A) : (Heap[A], Heap[A], Heap[A]) = if (leq(x, a)) if (leq(a, x)) (Empty[A], singletonWith(leq, x), Empty[A])
      else
        (singletonWith(leq, x), Empty[A], Empty[A])
      else
        (Empty[A], Empty[A], singletonWith(leq, x))
      t foldMap (x => f(x.value))
    })
  }

  /**Return a heap consisting of the least n elements of this heap. O(n log n) */
  def take(n: Int): Heap[A] = withList(_.take(n))

  /**Return a heap consisting of all the members of this heap except for the least n. O(n log n) */
  def drop(n: Int): Heap[A] = withList(_.drop(n))

  /**Split into two heaps, the first containing the n least elements, the second containing the n
   * greatest elements. O(n log n) */
  def splitAt(n: Int): (Heap[A], Heap[A]) = splitWithList(_.splitAt(n))

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
  def takeWhile(p: A => Boolean): Heap[A] =
    withList(_.takeWhile(p))

  /**Returns a heap consisting of the longest prefix of least elements of this heap that do not
   * satisfy the predicate. O(n log n) */
  def dropWhile(p: A => Boolean): Heap[A] =
    withList(_.dropWhile(p))

  /**Remove duplicate entries from the heap. O(n log n)*/
  def nub: Heap[A] = fold(Empty[A], (_, leq, t) => {
    val x = t.rootLabel.value
    val xs = deleteMin
    val zs = xs.dropWhile(leq(_, x))
    zs.nub.insertWith(leq, x)
  })

  /**Construct heaps from each element in this heap and union them together into a new heap. O(n)*/
  def flatMap[B: Order](f: A => Heap[B]): Heap[B] =
    fold(Empty[B], (_, _, t) => t foldMap (x => f(x.value)))

  /**Traverse the elements of the heap in sorted order and produce a new heap with applicative effects.
   * O(n log n)*/
  def traverse[F[_] : Applicative, B: Order](f: A => F[B]): F[Heap[B]] = {
    val F = Applicative[F]
    F.map(F.traverse(toEphemeralStream)(f))(fromCodata[EStream, B])
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B = {
    Foldable[EStream].foldRight(toEphemeralStream, z)(f)
  }

  private def withList(f: List[A] => List[A]) = {
    import std.list._
    fold(Empty[A], (_, leq, _) => fromDataWith(leq, f(toList)))
  }

  private[scalaz] def insertWith(f: (A, A) => Boolean, x: A) =
    fold(singletonWith(f, x), (s, _, t) => {
      val y = t.rootLabel.value
      if (f(x, y)) Heap(s + 1, f, Node(Ranked(0, x), (t ##:: emptyEphemeralStream)))
      else
        Heap(s + 1, f, Node(Ranked(0, y),
          skewInsert(f, Node(Ranked(0, x), emptyEphemeralStream[Tree[Ranked[A]]]), t.subForest)))
    })

  private def splitWithList(f: List[A] => (List[A], List[A])) = {
    import std.list._

    fold((Empty[A], Empty[A]), (_, leq, _) => {
      val g = (x: List[A]) => fromDataWith(leq, x)
      val x = f(toList)
      (g(x._1), g(x._2))
    })
  }

  override def toString = "<heap>"
}

object Heap extends HeapInstances {

  final case class Ranked[A](rank: Int, value: A)

  type Forest[A] = EStream[Tree[Ranked[A]]]
  type ForestZipper[A] = (Forest[A], Forest[A])

  /**The empty heap */
  object Empty {
    def apply[A]: Heap[A] = new Heap[A] {
      def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, Tree[Ranked[A]]) => B): B = empty
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
  def sort[F[_] : Foldable, A: Order](xs: F[A]): IList[A] = fromData(xs).toIList

  /**Heap sort */
  def sortWith[F[_] : Foldable, A](f: (A, A) => Boolean, xs: F[A]): IList[A] = fromDataWith(f, xs).toIList

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

  def apply[A](sz: Int, leq: (A, A) => Boolean, t: Tree[Ranked[A]]): Heap[A] = new Heap[A] {
    def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, Tree[Ranked[A]]) => B) =
      nonempty(sz, leq, t)
  }

  def unapply[A](h: Heap[A]): Option[(Int, (A, A) => Boolean, Tree[Ranked[A]])] =
    h.fold(None, (sz, leq, t) => Some((sz, leq, t)))

  private[scalaz] object impl {
    import Tree._

    def rightZ[A]: ForestZipper[A] => ForestZipper[A] = {
      case (path, x ##:: xs) => (x ##:: path, xs)
    }

    def adjustZ[A](f: Tree[Ranked[A]] => Tree[Ranked[A]]):
    ForestZipper[A] => ForestZipper[A] = {
      case (path, x ##:: xs) => (path, f(x) ##:: xs)
      case z                => z
    }

    def rezip[A]: ForestZipper[A] => Forest[A] = {
      case (s, xs) if s.isEmpty   => xs
      case (x ##:: path, xs) => rezip((path, x ##:: xs))
    }

    def rootZ[A]: ForestZipper[A] => A = {
      case (_, x ##:: _) => x.rootLabel.value
      case _            => sys.error("Heap.rootZ: empty zipper")
    }

    def zipper[A](xs: Forest[A]): ForestZipper[A] = (emptyEphemeralStream, xs)

    def emptyZ[A]: ForestZipper[A] = (emptyEphemeralStream, emptyEphemeralStream)

    def minZ[A](f: (A, A) => Boolean): Forest[A] => ForestZipper[A] = {
      case s if s.isEmpty => emptyZ
      case xs       => {
        val z = zipper(xs)
        minZp(f)(z, z)
      }
    }

    def minZp[A](leq: (A, A) => Boolean):
    (ForestZipper[A], ForestZipper[A]) => ForestZipper[A] = {
      case (lo, (_, ts)) if ts.isEmpty => lo
      case (lo, z)             => minZp(leq)(if (leq(rootZ(lo), rootZ(z))) lo else z, rightZ(z))
    }

    def heapify[A](leq: (A, A) => Boolean): Tree[Ranked[A]] => Tree[Ranked[A]] = {
      case n@Node(_, ts) if ts.isEmpty      => n
      case n@Node(Ranked(r, a), as) => {
        val (left, Node(Ranked(rp, ap), asp) ##:: right) = minZ(leq)(as)
        if (leq(a, ap)) n
        else
          Node(Ranked(r, ap), rezip((left, heapify(leq)(Node(Ranked(rp, a), asp)) ##:: right)))
      }
    }

    def singletonWith[A](f: (A, A) => Boolean, a: A) =
      Heap(1, f, Node(Ranked(0, a), emptyEphemeralStream[Tree[Ranked[A]]]))


    def rank[A](t: Tree[Ranked[A]]) = t.rootLabel.rank

    def skewLink[A](f: (A, A) => Boolean,
                    t0: Tree[Ranked[A]],
                    t1: Tree[Ranked[A]],
                    t2: Tree[Ranked[A]]): Tree[Ranked[A]] = (t0, t1, t2) match {
      case (Node(Ranked(r0, x0), cf0), Node(Ranked(r1, x1), cf1), Node(Ranked(r2, x2), cf2)) =>
        if (f(x1, x0) && f(x1, x2)) Node(Ranked(r1 + 1, x1), t0 ##:: t2 ##:: cf1)
        else
        if (f(x2, x0) && f(x2, x1)) Node(Ranked(r2 + 1, x2), t0 ##:: t1 ##:: cf2)
        else
          Node(Ranked(r1 + 1, x0), t1 ##:: t2 ##:: cf0)
    }

    def link[A](f: (A, A) => Boolean):
    (Tree[Ranked[A]], Tree[Ranked[A]]) => Tree[Ranked[A]] = {
      case (t1@Node(Ranked(r1, x1), cf1), t2@Node(Ranked(r2, x2), cf2)) =>
        if (f(x1, x2)) Node(Ranked(r1 + 1, x1), t2 ##:: cf1)
        else
          Node(Ranked(r2 + 1, x2), t1 ##:: cf2)
    }

    def skewInsert[A](f: (A, A) => Boolean, t: Tree[Ranked[A]], ts: Forest[A]): Forest[A] =
      ts match {
        case t1 ##:: t2 ##:: rest =>
          if (rank(t1) == rank(t2))
            skewLink(f, t, t1, t2) ##:: rest
          else (t ##:: ts)
        case _                  => t ##:: ts
      }

    def getMin[A](f: (A, A) => Boolean, trees: Forest[A]): (Tree[Ranked[A]], Forest[A]) =
      trees match {
        case (t ##:: ts) if ts.isEmpty => (t, emptyEphemeralStream)
        case t ##:: ts  => {
          val (tp, tsp) = getMin(f, ts)
          if (f(t.rootLabel.value, tp.rootLabel.value)) (t, ts) else (tp, t ##:: tsp)
        }
      }

    def splitForest[A]:
    (Int, Forest[A], Forest[A], Forest[A]) => (Forest[A], Forest[A], Forest[A]) = {
      case (0, zs, ts, f)                  => (zs, ts, f)
      case (1, zs, ts, (t ##:: tss)) if tss.isEmpty => (zs, t ##:: ts, emptyEphemeralStream)
      case (1, zs, ts, t1 ##:: t2 ##:: f)    =>
        if (rank(t2) == 0) (t1 ##:: zs, t2 ##:: ts, f)
        else
          (zs, t1 ##:: ts, t2 ##:: f)
      case (r, zs, ts, (t1 ##:: t2 ##:: cf)) =>
        if (rank(t1) == rank(t2)) (zs, t1 ##:: t2 ##:: ts, cf)
        else
        if (rank(t1) == 0) splitForest(r - 1, t1 ##:: zs, t2 ##:: ts, cf)
        else
          splitForest(r - 1, zs, t1 ##:: ts, t2 ##:: cf)
      case (_, _, _, _)                    => sys.error("Heap.splitForest: invalid arguments")
    }

    def skewMeld[A](f: (A, A) => Boolean, ts: Forest[A], tsp: Forest[A]) =
      unionUniq(f)(uniqify(f)(ts), uniqify(f)(tsp))

    def ins[A](f: (A, A) => Boolean, t: Tree[Ranked[A]]): Forest[A] => Forest[A] = {
      case s if s.isEmpty    => EphemeralStream(t)
      case (tp ##:: ts) => if (rank(t) < rank(tp)) t ##:: tp ##:: ts
      else
        ins(f, link(f)(t, tp))(ts)
    }

    def uniqify[A](f: (A, A) => Boolean): Forest[A] => Forest[A] = {
      case s if s.isEmpty   => emptyEphemeralStream
      case (t ##:: ts) => ins(f, t)(ts)
    }

    def unionUniq[A](f: (A, A) => Boolean): (Forest[A], Forest[A]) => Forest[A] = {
      case (s, ts) if s.isEmpty                         => ts
      case (ts, s) if s.isEmpty                         => ts
      case (tts1@(t1 ##:: ts1), tts2@(t2 ##:: ts2)) =>
        import std.anyVal._
        Order[Int].order(rank(t1), rank(t2)) match {
          case Ordering.LT => t1 ##:: unionUniq(f)(ts1, tts2)
          case Ordering.EQ => ins(f, link(f)(t1, t2))(unionUniq(f)(ts1, ts2))
          case Ordering.GT => t2 ##:: unionUniq(f)(tts1, ts2)
        }
    }
  }
}

sealed abstract class HeapInstances {
  implicit val heapInstance: Foldable[Heap] = new Foldable[Heap] with Foldable.FromFoldr[Heap] {
    def foldRight[A, B](fa: Heap[A], z: => B)(f: (A, => B) => B) = fa.foldRight(z)(f)
  }

  implicit def heapMonoid[A]: Monoid[Heap[A]] = new Monoid[Heap[A]] {
    def append(f1: Heap[A], f2: => Heap[A]) = f1 union f2
    def zero = Heap.Empty.apply
  }

  // implicit def heapEqual[A: Equal]: Equal[Heap[A]] = Equal.equalBy((_: Heap[A]).toStream)
  implicit def healEqual[A : Equal](implicit H : Foldable[Heap]): Equal[Heap[A]] = Equal[EStream[A]] contramap {H.toEphemeralStream(_: Heap[A])}
}
