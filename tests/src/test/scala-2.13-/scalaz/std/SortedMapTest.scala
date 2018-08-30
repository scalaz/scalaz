package scalaz
package std

import scala.collection.immutable.SortedMap
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties.band
import std.AllInstances._
import std.sortedMap.sortedMapBand

object SortedMapTest extends XMapTest[SortedMap, Order](std.sortedMap) {
  import dict._
  checkAll(band.laws[SortedMap[Int, ISet[Int]]])
}
