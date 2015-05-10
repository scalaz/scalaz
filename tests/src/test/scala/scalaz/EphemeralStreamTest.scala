package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class EphemeralStreamTest extends SpecLite {

  checkAll(equal.laws[EphemeralStream[Int]])
  checkAll(monadPlus.laws[EphemeralStream])
  checkAll(isEmpty.laws[EphemeralStream])
  checkAll(traverse.laws[EphemeralStream])

}
