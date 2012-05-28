package scalaz

// Returns a list in order of key insertion.
sealed trait InsertionMap[K, V] {
  private[scalaz] val assoc: Map[K, (V, Long)]
  private[scalaz] val next: Long

  def apply(k: K): Option[V] =
    get(k)

  def get(k: K): Option[V] =
    assoc get k map (_._1)

  def getOr(k: K, v: => V): V =
    get(k) getOrElse v

  def contains(k: K): Boolean =
    assoc contains k

  def ^+^(k: K, v: V): InsertionMap[K, V] =
    InsertionMap.build(assoc + ((k, (v, next))), next + 1L)

  def @-(k: K): (Option[V], InsertionMap[K, V]) =
    assoc get k match {
      case None => (None, this)
      case Some((w, _)) => (Some(w), InsertionMap.build(assoc - k, next))
    }

  def ^-^(k: K): InsertionMap[K, V] =
    @-(k)._2

  /** Returns a list with keys in the order of their insertion. */
  def toList: List[(K, V)] =
    assoc.toList sortWith {
      case ((_, (_, n)), (_, (_, o))) => n > o
    } map {
      case (k, (v, _)) => (k, v)
    }

  /** Returns a list with keys in the order of their insertion. */
  def keys: List[K] =
    toList map (_._1)

  def keySet: Set[K] =
    assoc.keySet

  def isEmpty: Boolean =
    assoc.isEmpty

  def size: Int =
    assoc.size

  override def toString: String =
    "Insertion" + (assoc mapValues (_._1) toString)
}

// forall, exists, filter, foreach, construction, partial lenses, keys, keySet, mapValues, size, isEmpty, empty, Equals, Show, Monoid
//
object InsertionMap {
  private[scalaz] def build[K, V](a: Map[K, (V, Long)], n: Long): InsertionMap[K, V] =
    new InsertionMap[K, V] {
      val assoc = a
      val next = n
    }

  def empty[K, V]: InsertionMap[K, V] =
    build(Map.empty, 0L)

  def apply[K, V](x: (K, V)*): InsertionMap[K, V] =
    x.foldRight(empty[K, V]) {
      case ((k, v), a) => a ^+^ (k, v)
    }
}