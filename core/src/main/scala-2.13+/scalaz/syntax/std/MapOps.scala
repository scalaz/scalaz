package scalaz
package syntax
package std

final class MapOps[K, A](private val self: Map[K, A]) extends AnyVal {
  import scalaz.std.{map => dict}
  def alter(k: K)(f: (Option[A] => Option[A])): Map[K, A] =
    dict.alter[K, A](self, k)(f)

  def intersectWithKey[B, C](m: Map[K, B])(f: (K, A, B) => C): Map[K, C] =
    dict.intersectWithKey[K, A, B, C](self, m)(f)

  def intersectWith[B, C](m: Map[K, B])(f: (A, B) => C): Map[K, C] =
    dict.intersectWith(self, m)(f)

  def mapKeys[K2](f: K => K2): Map[K2, A] =
    dict.mapKeys(self)(f)

  def unionWithKey(m: Map[K, A])(f: (K, A, A) => A): Map[K, A] =
    dict.unionWithKey[K, A](self, m)(f)

  def unionWith(m: Map[K, A])(f: (A, A) => A): Map[K, A] =
    dict.unionWith[K, A](self, m)(f)

  def insertWith(k: K, v: A)(f: (A, A) => A): Map[K, A] =
    dict.insertWith[K, A](self, k, v)(f)

  def getOrAdd[F[_]](k: K)(fa: => F[A])(implicit F: Applicative[F]): F[(Map[K, A], A)] =
    dict.getOrAdd[F, K, A](self, k)(fa)
}

trait ToMapOps {
  implicit def ToMapOpsFromMap[K, V](m: Map[K, V]): MapOps[K, V] = new MapOps[K, V](m)
}
