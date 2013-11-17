package scalaz
package std

import collection.immutable.SortedMap

import std.AllInstances._

object SortedMapTest extends XMapTest[SortedMap, Order](std.sortedMap)
