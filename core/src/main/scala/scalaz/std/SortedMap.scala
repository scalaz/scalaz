package scalaz
package std

import collection.immutable.SortedMap
import collection.generic.CanBuildFrom

sealed trait MapSubSortedMap {
  type XMap[K, V] = SortedMap[K, V]
  type BuildKeyConstraint[K] = Order[K]

  protected final def buildXMap[K, V, K2: BuildKeyConstraint, V2]
      : CanBuildFrom[XMap[K, V], (K2, V2), XMap[K2, V2]] = {
    implicit val so = Order[K2].toScalaOrdering
    implicitly
  }

  protected final def ab_+[K: BuildKeyConstraint, V
                         ](m: XMap[K, V], k: K, v: V): XMap[K, V] =
    m updated (k, v)

  protected final def ab_-[K: BuildKeyConstraint, V
                         ](m: XMap[K, V], k: K): XMap[K, V] =
    m - k
}

object sortedMap extends MapSubInstances with MapSubFunctions
    with MapSubSortedMap
