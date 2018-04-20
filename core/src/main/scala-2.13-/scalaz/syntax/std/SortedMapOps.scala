package scalaz
package syntax
package std

import collection.immutable.SortedMap

trait ToSortedMapOps {
  import scalaz.std.{sortedMap => dict}

  implicit def ToSortedMapOpsFromMap[K, V](m: SortedMap[K, V]): MapOps[SortedMap, dict.BuildKeyConstraint, K, V] = new MapOps[SortedMap, dict.BuildKeyConstraint, K, V](m)(dict)
}
