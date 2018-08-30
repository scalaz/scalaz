package scalaz
package syntax
package std

import scala.collection.immutable.SortedMap

final class SortedMapOps[K, A](private val self: SortedMap[K, A]) extends AnyVal {
  import scalaz.std.{sortedMap => dict}
  def alter(k: K)(f: (Option[A] => Option[A])): SortedMap[K, A] =
    dict.alter[K, A](self, k)(f)

  def intersectWithKey[B, C](m: SortedMap[K, B])(f: (K, A, B) => C)(implicit K: scala.Ordering[K]): SortedMap[K, C] =
    dict.intersectWithKey[K, A, B, C](self, m)(f)

  def intersectWith[B, C](m: SortedMap[K, B])(f: (A, B) => C)(implicit K: scala.Ordering[K]): SortedMap[K, C] =
    dict.intersectWith(self, m)(f)

  def mapKeys[K2: scala.Ordering](f: K => K2): SortedMap[K2, A] =
    dict.mapKeys(self)(f)

  def unionWithKey(m: SortedMap[K, A])(f: (K, A, A) => A)(implicit K: scala.Ordering[K]): SortedMap[K, A] =
    dict.unionWithKey[K, A](self, m)(f)

  def unionWith(m: SortedMap[K, A])(f: (A, A) => A)(implicit K: scala.Ordering[K]): SortedMap[K, A] =
    dict.unionWith[K, A](self, m)(f)

  def insertWith(k: K, v: A)(f: (A, A) => A): SortedMap[K, A] =
    dict.insertWith[K, A](self, k, v)(f)

  def getOrAdd[F[_]](k: K)(fa: => F[A])(implicit F: Applicative[F]): F[(SortedMap[K, A], A)] =
    dict.getOrAdd[F, K, A](self, k)(fa)
}

trait ToSortedMapOps {
  implicit def ToSortedMapOpsFromSortedMap[K, V](m: SortedMap[K, V]): SortedMapOps[K, V] = new SortedMapOps[K, V](m)
}
