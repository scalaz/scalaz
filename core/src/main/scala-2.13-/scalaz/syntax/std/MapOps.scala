package scalaz
package syntax
package std

final class MapOps[Map[_, _], BKC[_], K, A](self: Map[K, A])
                  (dict: scalaz.std.MapSubFunctions{
                     type XMap[A, B] = Map[A, B]
                     type BuildKeyConstraint[A] = BKC[A]
                   }) {
  final def alter(k: K)(f: (Option[A] => Option [A]))(implicit bk: BKC[K]): Map[K, A] = dict.alter(self, k)(f)
  final def intersectWithKey[B, C](m: Map[K, B])(f: (K, A, B) => C)(implicit bk: BKC[K]): Map[K, C] = dict.intersectWithKey(self, m)(f)
  final def intersectWith[B, C](m: Map[K, B])(f: (A, B) => C)(implicit bk: BKC[K]): Map[K, C] = dict.intersectWith(self, m)(f)
  final def mapKeys[K2: BKC](f: K => K2): Map[K2, A] = dict.mapKeys(self)(f)
  final def unionWithKey(m: Map[K, A])(f: (K, A, A) => A)(implicit bk: BKC[K]): Map[K, A] = dict.unionWithKey(self, m)(f)
  final def unionWith(m: Map[K, A])(f: (A, A) => A)(implicit bk: BKC[K]): Map[K, A] = dict.unionWith(self, m)(f)
  final def insertWith(k: K, v: A)(f: (A, A) => A)(implicit bk: BKC[K]): Map[K, A] = dict.insertWith(self, k, v)(f)
  final def getOrAdd[F[_]](k: K)(fa: => F[A])(implicit F: Applicative[F], bk: BKC[K]): F[(Map[K, A], A)] = dict.getOrAdd(self, k)(fa)
}

trait ToMapOps {
  import scalaz.std.{map => dict}

  implicit def ToMapOpsFromMap[K, V](m: Map[K, V]): MapOps[Map, dict.BuildKeyConstraint, K, V] = new MapOps[Map, dict.BuildKeyConstraint, K, V](m)(dict)
}

