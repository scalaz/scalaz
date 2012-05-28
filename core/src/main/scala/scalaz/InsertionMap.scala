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

  def ++(q: InsertionMap[K, V]): InsertionMap[K, V] = {
    val s = q.assoc.toList sortWith {
      case ((_, (_, n)), (_, (_, o))) => n < o
    }

    s.foldLeft(this){ case (m, (k, (v, _))) => m ^+^ (k, v) }
  }

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

  def forall(p: (K, V) => Boolean): Boolean =
    assoc forall {
      case (k, (v, _)) => p(k, v)
    }

  def exists(p: (K, V) => Boolean): Boolean =
    assoc exists {
      case (k, (v, _)) => p(k, v)
    }

  def filter(p: V => Boolean): InsertionMap[K, V] =
    InsertionMap.build(assoc filter {
      case (_, (v, _)) => p(v)
    }, next)

  def map[W](f: V => W): InsertionMap[K, W] =
    InsertionMap.build(assoc mapValues {
      case (v, n) => (f(v), n)
    }, next)


  override def toString: String =
    "InsertionMap(" + (toList mkString ", ") + ")"

}

object InsertionMap extends InsertionMapFunctions with InsertionMapInstances

trait InsertionMapFunctions {
  private[scalaz] def build[K, V](a: Map[K, (V, Long)], n: Long): InsertionMap[K, V] =
    new InsertionMap[K, V] {
      val assoc = a
      val next = n
    }

  def empty[K, V]: InsertionMap[K, V] =
    build(Map.empty, 0L)

  def apply[K, V](x: (K, V)*): InsertionMap[K, V] =
    x.reverse.foldLeft(empty[K, V]) {
      case (a, (k, v)) => a ^+^ (k, v)
    }

  def insertionMapL[K, V](k: K): InsertionMap[K, V] @> Option[V] =
    Lens(q => Costate(_ match {
      case None => q ^-^ k
      case Some(v) => q ^+^ (k, v)
    }, q get k))

  def insertionMapPL[K, V](k: K): InsertionMap[K, V] @?> V =
    PLens.somePLens compose ~insertionMapL(k)

}

trait InsertionMapInstances {
  import std.AllInstances._

  implicit def insertionMap[K]: Functor[({type λ[α]=InsertionMap[K, α]})#λ] =
    new Functor[({type λ[α]=InsertionMap[K, α]})#λ] {
      def map[A, B](a: InsertionMap[K, A])(f: A => B) =
        a map f
    }

  implicit def insertionMapEqual[K: Equal, V: Equal]: Equal[InsertionMap[K, V]] =
    Equal[List[(K, V)]] contramap (_.toList)

  implicit def insertionMapShow[K: Show, V: Show]: Show[InsertionMap[K, V]] =
    Show.show(q =>
      "InsertionMap(" + (q.toList map (Show[(K, V)] shows _) mkString ", ") + ")" toList)

  implicit def insertionMapMonoid[K, V]: Monoid[InsertionMap[K, V]] =
    new Monoid[InsertionMap[K, V]] {
      def append(f1: InsertionMap[K, V], f2: => InsertionMap[K, V]) = f1 ++ f2
      def zero = InsertionMap.empty
    }

}
