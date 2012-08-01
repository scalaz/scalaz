package scalaz
package syntax
package std

import scalaz.std.{ map => dict }

trait MapOps[K, A] extends Ops[Map[K, A]] {
  final def intersectWithKey[B, C](m: Map[K, B])(f: (K, A, B) => C): Map[K, C] = dict.intersectWithKey(self, m)(f)
  final def intersectWith[B, C](m: Map[K, B])(f: (A, B) => C): Map[K, C] = dict.intersectWith(self, m)(f)
  final def unionWithKey(m: Map[K, A])(f: (K, A, A) => A): Map[K, A] = dict.unionWithKey(self, m)(f)
  final def unionWith(m: Map[K, A])(f: (A, A) => A): Map[K, A] = dict.unionWith(self, m)(f)
}

trait ToMapOps {
  implicit def ToMapOpsFromMap[K, V](m: Map[K, V]): MapOps[K, V] = new MapOps[K, V] { val self = m }
}

