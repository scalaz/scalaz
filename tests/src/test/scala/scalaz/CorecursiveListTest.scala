package scalaz

import org.scalacheck.Prop.{exists, forAll, propBoolean}

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.anyVal._

object CorecursiveListTest extends SpecLite {
  type CL[A] = CorecursiveList[A]
  checkAll(monadPlus.laws[CL])
  checkAll(foldable.laws[CL])
  checkAll(zip.laws[CL])
  checkAll(monoid.laws[CL[Int]])
  checkAll(order.laws[CL[Int]])

  "inequality exists" ! forAll {(a: CL[Int]) =>
    exists {(b: CL[Int]) =>
      propBoolean(!Equal[CL[Int]].equal(a, b))
    }
  }

  object instances {
    def equalAmbiguous[A: Order] = Equal[CL[A]]
  }
}
