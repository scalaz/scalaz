package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class LazyOptionTest extends Spec {
  checkAll(equal.laws[LazyOption[Int]])
  checkAll(monadPlus.laws[LazyOption])
  checkAll(cobind.laws[LazyOption])
  checkAll(traverse.laws[LazyOption])
}
