package scalaz

import collection.immutable.TreeMap

// Returns a list in order of key insertion.
sealed trait InsertionMap[K, V] {
  private[scalaz] val assoc: TreeMap[K, (V, Long)]
  private[scalaz] val next: Long

  def get(k: K): Option[V] =
    assoc get k map (_._1)

  def getOr(k: K, v: => V): V =
    get(k) getOrElse v

  def contains(k: K): Boolean =
    assoc contains k

  def +(k: K, v: V): InsertionMap[K, V] =
    InsertionMap(assoc + ((k, (v, next))), next + 1L)

  def ++(m: InsertionMap[K, V]): InsertionMap[K, V] =
    InsertionMap(assoc ++ m.assoc, next + m.next)

  def @-(k: K): (Option[V], InsertionMap[K, V]) =
    assoc get k match {
      case None => (None, this)
      case Some((w, _)) => (Some(w), InsertionMap(assoc - k, next))
    }

  def -(k: K): InsertionMap[K, V] =
    @-(k)._2

  /** Returns a list with keys in the order of their insertion. */
  def toList: List[(K, V)] =
    assoc.toList sortWith {
      case ((_, (_, n)), (_, (_, o))) => n < o
    } map {
      case (k, (v, _)) => (k, v)
    }

  /** Returns a list with keys in the order of their insertion. */
  def keys: List[K] =
    toList map (_._1)

  def keySet: Set[K] =
    assoc.keySet
}

// forall, exists, filter, foreach, construction, partial lenses, keys, keySet, mapValues, size, isEmpty, empty, Equals, Show, Monoid
//
object InsertionMap {
  private[scalaz] def apply[K, V](a: TreeMap[K, (V, Long)], n: Long): InsertionMap[K, V] =
    new InsertionMap[K, V] {
      val assoc = a
      val next = n
    }
}