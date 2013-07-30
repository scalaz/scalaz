package scalaz

/** Returns a list in order of key insertion. */
final class InsertionMap[K, V] private[scalaz](
  private[scalaz] val assoc: Map[K, (V, Long)],
  private[scalaz] val next: Long) {

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

  def toMap: Map[K, V] =
    assoc.map({ case (k, (v, _)) => (k, v) })

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

  override def equals(a: Any): Boolean =
    a.isInstanceOf[InsertionMap[_, _]] &&
      toMap == a.asInstanceOf[InsertionMap[_, _]].toMap

  override def hashCode: Int =
    toMap.hashCode

  override def toString: String =
    "InsertionMap(" + (toList mkString ", ") + ")"
}

object InsertionMap extends InsertionMapInstances with InsertionMapFunctions

trait InsertionMapFunctions {
  private[scalaz] def build[K, V](a: Map[K, (V, Long)], n: Long): InsertionMap[K, V] =
    new InsertionMap[K, V](a, n)

  def empty[K, V]: InsertionMap[K, V] =
    build(Map.empty, 0L)

  def apply[K, V](x: (K, V)*): InsertionMap[K, V] =
    x.foldLeft(empty[K, V]) {
      case (a, (k, v)) => a ^+^ (k, v)
    }

  def insertionMapL[K, V](k: K): InsertionMap[K, V] @> Option[V] =
    Lens(q => Store(_ match {
      case None => q ^-^ k
      case Some(v) => q ^+^ (k, v)
    }, q get k))

  def insertionMapPL[K, V](k: K): InsertionMap[K, V] @?> V =
    PLens.somePLens compose ~insertionMapL(k)

}

sealed abstract class InsertionMapInstances {
  import Scalaz._

  implicit def insertionTraverse[K]: Traverse[({type λ[α]=InsertionMap[K, α]})#λ] =
    new Traverse[({type λ[α]=InsertionMap[K, α]})#λ] {
      def traverseImpl[F[_], A, B](m: InsertionMap[K, A])(f: A => F[B])(implicit F: Applicative[F]) = {
        m.toList.foldLeft(F.point(InsertionMap.empty[K, B]))({
          case (acc, (k, v)) => F.apply2(acc, f(v))(_ ^+^ (k, _))
        })
      }
    }

  implicit def insertionMap[K]: Functor[({type λ[α]=InsertionMap[K, α]})#λ] =
    new Functor[({type λ[α]=InsertionMap[K, α]})#λ] {
      def map[A, B](a: InsertionMap[K, A])(f: A => B) =
        a map f
    }

  implicit def insertionMapEqual[K, V]: Equal[InsertionMap[K, V]] =
    Equal.equalA

  import Cord._

  implicit def insertionMapShow[K: Show, V: Show]: Show[InsertionMap[K, V]] =
    Show.show(q => Cord("InsertionMap(", Cord.mkCord(", ", q.toList.map(Show[(K, V)] show _):_*), ")"))

}
