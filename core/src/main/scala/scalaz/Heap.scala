package scalaz

/** An efficient, asymptotically optimal, implementation of priority queues
  * extended with support for efficient size.
  *
  * The implementation of 'Heap' is based on bootstrapped skew binomial heaps 
  * as described by:
  * G. Brodal and C. Okasaki , "Optimal Purely Functional Priority Queues",
  *    Journal of Functional Programming 6:839-857 (1996),
  * 
  * Based on the heaps Haskell library by Edward Kmett
  *
  */

import Scalaz._

case class Ranked[A](rank: Int, value: A)

trait Heap[A] {
  import Heap._
  
  def fold[B](empty: => B, nonempty: (Int, (A, A) => Boolean, Tree[Ranked[A]]) => B): B = 
    this match {
      case NilHeap() => empty
      case ConsHeap(s, o, h) => nonempty(s, o, h)
    }

  /** Is the heap empty? O(1)*/
  def isEmpty = fold(true, (_, _, _) => false)

  /** The number of elements in the heap. O(1)*/
  def size = fold(0, (s, _, _) => s)

  /** Insert a new value into the heap. O(1)*/
  def insert(x: A)(implicit o: Order[A]) = insertWith(_ lte _, x)
  
  private def insertWith(f: (A, A) => Boolean, x: A) =
    fold(singletonWith(f, x), (s, _, t) => {
      val y = t.rootLabel.value
      if (f(x, y)) ConsHeap(s + 1, f, node(Ranked(0, x), Stream(t))) else
         ConsHeap(s + 1, f, node(Ranked(0, y),
                  skewInsert(f, node(Ranked(0, x), Stream()), t.subForest)))
    })

  /** Meld the values from two heaps into one heap. O(1)*/
  def union(as: Heap[A]) = (this, as) match {
    case (NilHeap(), q) => q
    case (q, NilHeap()) => q
    case (ConsHeap(s1, leq, t1@Node(Ranked(r1, x1), f1)),
          ConsHeap(s2, _, t2@Node(Ranked(r2, x2), f2))) =>
      if (leq(x1, x2))
        ConsHeap(s1 + s2, leq, node(Ranked(0, x1), skewInsert(leq, t2, f1)))
      else
        ConsHeap(s1 + s2, leq, node(Ranked(0, x2), skewInsert(leq, t1, f2)))
  }

  /** Split the heap into the minimum element and the remainder.*/
  def uncons: Option[A :&: Heap[A]] =
    fold(None, (_, _, t) => Some(lazyTuple(t.rootLabel.value, deleteMin)))

  /** Get the minimum key on the (nonempty) heap. O(1) */
  def minimum: A = fold(error("Heap.minimum: empty heap"), (_, _, t) => t.rootLabel.value)

  /** Delete the minimum key from the heap and return the resulting heap. */
  def deleteMin: Heap[A] = {
    fold(empty[A], (s, leq, t) => t match {
      case Node(_, Stream()) => empty[A]
      case Node(_, f0) => {
        val (Node(Ranked(r, x), cf), ts2) = getMin(leq, f0)
        val (zs, ts1, f1) = splitForest(r, Stream(), Stream(), cf)
        val f2 = skewMeld(leq, skewMeld(leq, ts1, ts2), f1)
        val f3 = zs.foldRight(f2)(skewInsert(leq, _, _))
        heap(s - 1, leq, node(Ranked(0, x), f3))
      }
    })
  }
}
case class NilHeap[A]() extends Heap[A]
case class ConsHeap[A](sz: Int, ord: (A, A) => Boolean, heap: Tree[Ranked[A]]) extends Heap[A]

object Heap {
  type Forest[A] = Stream[Tree[Ranked[A]]]

  /** The empty heap */
  def empty[A]: Heap[A] = NilHeap()

  /** A nonempty heap */
  def heap[A] = ConsHeap[A](_, _, _)

  private def singletonWith[A](f: (A, A) => Boolean, a: A) =
    heap(1, f, node(Ranked(0, a), Stream()))
  
  /** A heap with one element. */
  def singleton[A:Order](a: A) = singletonWith[A](_ lte _, a)

  /** Create a heap consisting of multiple copies of the same value. O(log n) */
  def replicate[A:Order](a: A, i: Int): Heap[A] = {
    def f(x: Heap[A], y: Int): Heap[A] =
      if (y % 2 == 0) f(x union x, y / 2) else
      if (y == 1) x else
        g(x union x, (y - 1) / 2, x)
    def g(x: Heap[A], y: Int, z: Heap[A]): Heap[A] =
      if (y % 2 == 0) g(x union x, y / 2, z) else
      if (y == 1) x union z else
        g(x union x, (y - 1) / 2, x union z)
    if (i < 0) error("Heap.replicate: negative length") else
    if (i == 0) empty else
       f(singleton(a), i)
  }

  private def rank[A](t: Tree[Ranked[A]]) = t.rootLabel.rank

  private def skewLink[A](f: (A, A) => Boolean,
               t0: Tree[Ranked[A]],
               t1: Tree[Ranked[A]],
               t2: Tree[Ranked[A]]): Tree[Ranked[A]] = (t0, t1, t2) match {
    case (Node(Ranked(r0, x0), cf0), Node(Ranked(r1, x1), cf1), Node(Ranked(r2, x2), cf2)) =>
      if (f(x1, x0) && f(x1, x2)) Node(Ranked(r1 + 1, x1), t0 #:: t2 #:: cf1) else
      if (f(x2, x0) && f(x2, x1)) Node(Ranked(r2 + 1, x2), t0 #:: t1 #:: cf2) else
       Node(Ranked(r1 + 1, x0), t1 #:: t2 #:: cf0)
  }
  private def link[A](f: (A, A) => Boolean):
    (Tree[Ranked[A]], Tree[Ranked[A]]) => Tree[Ranked[A]] = {
      case (t1@Node(Ranked(r1, x1), cf1), t2@Node(Ranked(r2, x2), cf2)) =>
        if (f(x1, x2)) Node(Ranked(r1 + 1, x1), t2 #:: cf1) else
           Node(Ranked(r2 + 1, x2), t1 #:: cf2)
    }
  private def skewInsert[A](f: (A, A) => Boolean, t: Tree[Ranked[A]], ts: Forest[A]): Forest[A] =
    ts match {
      case t1 #:: t2 #:: rest =>
        if (rank(t1) == rank(t2))
          skewLink(f, t, t1, t2) #:: rest
        else (t #:: ts)
      case _ => t #:: ts
    }
  private def getMin[A](f: (A, A) => Boolean, trees: Forest[A]): (Tree[Ranked[A]], Forest[A]) =
    trees match {
      case Stream(t) => (t, Stream())
      case t #:: ts => {
        val (tp, tsp) = getMin(f, ts)
        if (f(t.rootLabel.value, tp.rootLabel.value)) (t, ts) else (tp, t #:: tsp)
      }
    }
  private def splitForest[A]:
    (Int, Forest[A], Forest[A], Forest[A]) => (Forest[A], Forest[A], Forest[A]) = {
      case (0, zs, ts, f) => (zs, ts, f)
      case (1, zs, ts, Stream(t)) => (zs, t #:: ts, Stream())
      case (1, zs, ts, t1 #:: t2 #:: f) =>
        if (rank(t2) == 0) (t1 #:: zs, t2 #:: ts, f) else
          (zs, t1 #:: ts, t2 #:: f)
      case (r, zs, ts, (t1 #:: t2 #:: cf)) =>
        if (rank(t1) == rank(t2)) (zs, t1 #:: t2 #:: ts, cf) else
        if (rank(t1) == 0) splitForest(r-1, t1 #:: zs, t2 #:: ts, cf) else
          splitForest(r-1, zs, t1 #:: ts, t2 #:: cf)
      case (_, _, _, _) => error("Heap.splitForest: invalid arguments")
    }
  private def skewMeld[A](f: (A, A) => Boolean, ts: Forest[A], tsp: Forest[A]) =
    unionUniq(f)(uniqify(f)(ts), uniqify(f)(tsp))
  private def ins[A](f: (A, A) => Boolean, t: Tree[Ranked[A]]): Forest[A] => Forest[A] = {
    case Stream() => Stream(t)
    case (tp #:: ts) => if (rank(t) < rank(tp)) t #:: tp #:: ts else
                        ins(f, link(f)(t, tp))(ts)
  }
  private def uniqify[A](f: (A, A) => Boolean): Forest[A] => Forest[A] = {
    case Stream() => Stream()
    case (t #:: ts) => ins(f, t)(ts)
  }
  private def unionUniq[A](f: (A, A) => Boolean): (Forest[A], Forest[A]) => Forest[A] = {
    case (Stream(), ts) => ts
    case (ts, Stream()) => ts
    case (tts1@(t1 #:: ts1), tts2@(t2 #:: ts2)) => rank(t1) ?|? rank(t2) match {
      case LT => t1 #:: unionUniq(f)(ts1, tts2)
      case EQ => ins(f, link(f)(t1, t2))(unionUniq(f)(ts1, ts2))
      case GT => t2 #:: unionUniq(f)(tts1, ts2)
    }
  }
}

