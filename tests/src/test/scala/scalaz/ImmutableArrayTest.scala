package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.anyVal._

object ImmutableArrayTest extends SpecLite {

  "Issue #812" in {
    val xs = ImmutableArray.fromArray(Array("test"))
    val t = xs.tail
    t.toArray.toList must_== Array[String]().toList
  }

  checkAll(equal.laws[ImmutableArray[Int]])
  checkAll(FoldableTests.anyAndAllLazy[ImmutableArray])

}
