package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object LazyOptionTest extends SpecLite {
  checkAll(equal.laws[LazyOption[Int]])
  checkAll(monadPlus.laws[LazyOption])
  checkAll(cobind.laws[LazyOption])
  checkAll(traverse.laws[LazyOption])
  checkAll(zip.laws[LazyOption])
  checkAll(align.laws[LazyOption])
}
