package scalaz

import scala.annotation.tailrec
import scalaz.syntax.order._
import scalaz.syntax.show._

import PrioritySearchQueue.Summary

/** Container whose elements have two different orders: by priority and by lookup key.
  *
  * Supports efficient implementation of the following operations:
  * - insertion (O(log n)),
  * - lookup by key (O(log n)),
  * - deletion by key (O(log n)),
  * - access to minimal-by-priority element (O(1)),
  * - deletion of minimal-by-priority element (O(log n)).
  *
  * Implemented using [[FingerTree]].
  *
  * @tparam A element type
  * @tparam P priority of an element. Multiple elements can have the same priority.
  * @tparam K lookup key. Unique—the queue holds at most one element with any given key.
  */
class PrioritySearchQueue[A, P, K] private(
    private val tree: FingerTree[Summary[A, P, K], A],
    priorityOf: A => P,
    keyOf: A => K
) {
  def isEmpty: Boolean = tree.isEmpty
  def nonEmpty: Boolean = !tree.isEmpty
  def size: Int = tree.measure.map(_.size).getOrElse(0)

  /** Returns an element with minimum priority, or [[Maybe.Empty]] if this queue is empty.
    *
    * If there are multiple elements with the same minimum priority,
    * it is unspecified which of them is returned.
    */
  def minimum: Maybe[A] = tree.measure.map(_.minByPrio)

  def minimumPriority: Maybe[P] = tree.measure.map(_.minPrio)

  /** Removes an element with minimum priority.
    *
    * If there are multiple elements with the same minimum priority,
    * it is guaranteed to remove the one returned by [[minimum]].
    *
    * ''O(log(n))''.
    *
    * If this queue is empty, returns `this`.
    */
  def deleteMin(implicit P: Order[P]): PrioritySearchQueue[A, P, K] =
    tree.measure match {
      case Maybe.Empty() =>
        this
      case Maybe.Just(summary) =>
        val (tree1, tree2) = tree.split(_.minPrio === summary.minPrio)
        assert(!tree2.isEmpty) // the element with the minimal priority must be present in the tree
        new PrioritySearchQueue(tree1 <++> tree2.tail, priorityOf, keyOf)
    }

  // hack to get an empty tree without calling FingerTree.empty, which requires a Reducer
  private def emptyTree = tree.split(_ => false)._2

  /** Removes elements with priority less than `p`.
    *
    * ''O(min(k*log(n), (n-k)*log(n), n))'', where ''k'' is the number of actually removed elements.
    * For large ''k'', this method is more efficient than iterated [[deleteMin]].
    *
    * To also return the removed elements, use [[splitBeforePrio]].
    */
  def deleteLt(p: P)(implicit P: Order[P]): PrioritySearchQueue[A, P, K] = {
    @tailrec def go(
        accGte: FingerTree[Summary[A, P, K], A],
        tree: FingerTree[Summary[A, P, K], A]
    ): PrioritySearchQueue[A, P, K] =
      if (tree.isEmpty) {
        new PrioritySearchQueue(accGte, priorityOf, keyOf)
      } else {
        val (_, tree1) = tree.split(_.maxPrio >= p)
        val (gte, tree2) = tree1.split(_.minPrio < p)
        go(accGte <++> gte, tree2)
      }

    go(emptyTree, tree)
  }

  /** Splits this queue into elements with priority less than `p`
    * and elements with priority greater than or equal to `p`.
    */
  def splitBeforePrio(p: P)(implicit P: Order[P]): (PrioritySearchQueue[A, P, K], PrioritySearchQueue[A, P, K]) = {
    @tailrec def go(
        accLt: FingerTree[Summary[A, P, K], A],
        accGte: FingerTree[Summary[A, P, K], A],
        tree: FingerTree[Summary[A, P, K], A]
    ): (PrioritySearchQueue[A, P, K], PrioritySearchQueue[A, P, K]) =
      if (tree.isEmpty) {
        (new PrioritySearchQueue(accLt, priorityOf, keyOf), new PrioritySearchQueue(accGte, priorityOf, keyOf))
      } else {
        val (lt, tree1) = tree.split(_.maxPrio >= p)
        val (gte, tree2) = tree1.split(_.minPrio < p)
        go(accLt <++> lt, accGte <++> gte, tree2)
      }

    go(emptyTree, emptyTree, tree)
  }

  /** Removes elements with priority less than or equal to `p`.
    *
    * ''O(min(k*log(n), (n-k)*log(n), n))'', where ''k'' is the number of actually removed elements.
    * For large ''k'', this method is more efficient than iterated [[deleteMin]].
    *
    * To also return the removed elements, use [[splitAfterPrio]].
    */
  def deleteLte(p: P)(implicit P: Order[P]): PrioritySearchQueue[A, P, K] = {
    @tailrec def go(
        accGt: FingerTree[Summary[A, P, K], A],
        tree: FingerTree[Summary[A, P, K], A]
    ): PrioritySearchQueue[A, P, K] =
      if (tree.isEmpty) {
        new PrioritySearchQueue(accGt, priorityOf, keyOf)
      } else {
        val (_, tree1) = tree.split(_.maxPrio > p)
        val (gt, tree2) = tree1.split(_.minPrio <= p)
        go(accGt <++> gt, tree2)
      }

    go(emptyTree, tree)
  }

  /** Splits this queue into elements with priority less than or equal to `p`
    * and elements with priority greater than `p`.
    */
  def splitAfterPrio(p: P)(implicit P: Order[P]): (PrioritySearchQueue[A, P, K], PrioritySearchQueue[A, P, K]) = {
    @tailrec def go(
        accLte: FingerTree[Summary[A, P, K], A],
        accGt: FingerTree[Summary[A, P, K], A],
        tree: FingerTree[Summary[A, P, K], A]
    ): (PrioritySearchQueue[A, P, K], PrioritySearchQueue[A, P, K]) =
      if (tree.isEmpty) {
        (new PrioritySearchQueue(accLte, priorityOf, keyOf), new PrioritySearchQueue(accGt, priorityOf, keyOf))
      } else {
        val (lte, tree1) = tree.split(_.maxPrio > p)
        val (gt, tree2) = tree1.split(_.minPrio <= p)
        go(accLte <++> lte, accGt <++> gt, tree2)
      }

    go(emptyTree, emptyTree, tree)
  }

  /** Inserts the given element into this queue.
    *
    * If an element with the same key is already present in this queue, it is replaced by `elem`
    * and the replaced element is returned in the second part of the returned pair.
    */
  def insert(elem: A)(implicit K: Order[K]): (PrioritySearchQueue[A, P, K], Maybe[A]) = {
    val (tree1, tree2) = tree.split(_.maxKey >= keyOf(elem))
    val (newTree, replacedElem) =
      if (tree2.isEmpty) {
        (tree1 :+ elem, Maybe.empty[A])
      } else if (keyOf(tree2.head) === keyOf(elem)) {
        (tree1 <++> (elem |-: tree2), Maybe.just(tree2.head))
      } else {
        (tree1.add1(elem, tree2), Maybe.empty[A])
      }
    (new PrioritySearchQueue(newTree, priorityOf, keyOf), replacedElem)
  }

  /** Looks up an element by key. */
  def get(key: K)(implicit K: Order[K]): Maybe[A] = {
    val (tree1, tree2) = tree.split(_.maxKey >= key)
    if (tree2.isEmpty) {
      Maybe.empty
    } else if (keyOf(tree2.head) === key) {
      Maybe.just(tree2.head)
    } else {
      Maybe.empty
    }
  }

  /** Removes an element by key.
    *
    * Returns the removed element, if any, in the second part of the returned pair.
    */
  def removeByKey(key: K)(implicit K: Order[K]): (PrioritySearchQueue[A, P, K], Maybe[A]) = {
    val (tree1, tree2) = tree.split(_.maxKey >= key)
    if (tree2.isEmpty) {
      (this, Maybe.empty)
    } else if (keyOf(tree2.head) === key) {
      (new PrioritySearchQueue(tree1 <++> tree2.tail, priorityOf, keyOf), Maybe.just(tree2.head))
    } else {
      (this, Maybe.empty)
    }
  }

  def containsKey(key: K)(implicit K: Order[K]): Boolean = {
    val (tree1, tree2) = tree.split(_.maxKey >= key)
    !tree2.isEmpty && (keyOf(tree2.head) === key)
  }

  def toUnsortedList: List[A] = tree.toList
  def toUnsortedIList: IList[A] = tree.toIList
}

object PrioritySearchQueue {
  def empty[A, P: Order, K: Order](priorityOf: A => P, keyOf: A => K): PrioritySearchQueue[A, P, K] = {
    def summaryOf(a: A): Summary[A, P, K] =
      Summary(1, priorityOf(a), priorityOf(a), a, a, keyOf(a), keyOf(a))

    implicit val reducer: Reducer[A, Summary[A, P, K]] =
      Reducer.unitReducer(summaryOf)

    new PrioritySearchQueue[A, P, K](FingerTree.empty, priorityOf, keyOf)
  }

  private[PrioritySearchQueue] case class Summary[A, P, K](
      size: Int,
      minPrio: P,
      maxPrio: P,
      minByPrio: A,
      maxByPrio: A,
      minKey: K,
      maxKey: K
  )

  private[PrioritySearchQueue] object Summary {
    implicit def summarySemigroup[A, P, K](implicit P: Order[P], K: Order[K]): Semigroup[Summary[A, P, K]] =
      new Semigroup[Summary[A, P, K]] {
        override def append(s1: Summary[A, P, K], s2: => Summary[A, P, K]): Summary[A, P, K] = {
          val (minPrio, minByPrio) =
            if (s1.minPrio <= s2.minPrio) (s1.minPrio, s1.minByPrio)
            else (s2.minPrio, s2.minByPrio)
          val (maxPrio, maxByPrio) =
            if (s1.maxPrio >= s2.maxPrio) (s1.maxPrio, s1.maxByPrio)
            else (s2.maxPrio, s2.maxByPrio)
          Summary(
            s1.size + s2.size,
            minPrio,
            maxPrio,
            minByPrio,
            maxByPrio,
            s1.minKey min s2.minKey,
            s1.maxKey max s2.maxKey
          )
        }
      }
  }

  implicit def showInstance[A: Show, P, K]: Show[PrioritySearchQueue[A, P, K]] = new Show[PrioritySearchQueue[A, P, K]] {
    override def show(q: PrioritySearchQueue[A, P, K]): Cord =
      Cord("PrioritySearchQueue(") :: (q.tree.foldRight(Cord(")")) { (a, acc) => a.show :: Cord(", ") :: acc })
  }
}
