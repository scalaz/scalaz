package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StreamTest extends Spec {
  checkAll(equal.laws[Stream[Int]])
  checkAll(monoid.laws[Stream[Int]])
  checkAll(monadPlus.laws[Stream])
  checkAll(traverse.laws[Stream])
}
