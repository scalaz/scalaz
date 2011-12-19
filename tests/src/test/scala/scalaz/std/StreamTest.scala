package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StreamTest extends Spec {
  checkAll("Stream", equal.laws[Stream[Int]])
  checkAll("Stream", monoid.laws[Stream[Int]])
  checkAll("Stream", monadPlus.laws[Stream])
  checkAll("Stream", traverse.laws[Stream])
}
