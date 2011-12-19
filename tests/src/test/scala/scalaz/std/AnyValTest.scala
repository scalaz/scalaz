package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

class AnyValTest extends Spec {

  checkAll("Unit", order.laws[Unit])
  checkAll("Boolean", order.laws[Boolean].withProp("benchmark", order.scalaOrdering[Boolean]))
  checkAll("Char", order.laws[Char].withProp("benchmark", order.scalaOrdering[Char]))
  checkAll("Short", order.laws[Short].withProp("benchmark", order.scalaOrdering[Short]))
  checkAll("Int", order.laws[Int].withProp("benchmark", order.scalaOrdering[Int]))
  checkAll("Long", order.laws[Long].withProp("benchmark", order.scalaOrdering[Long]))
  checkAll("Float", order.laws[Float].withProp("benchmark", order.scalaOrdering[Float]))
  checkAll("Int @@ Multiplication", order.laws[Int @@ Multiplication])
  checkAll("Boolean @@ Conjunction", order.laws[Boolean @@ Conjunction])
  checkAll("Char @@ Multiplication", order.laws[Char @@ Multiplication])
  checkAll("Byte @@ Multiplication", order.laws[Byte @@ Multiplication])
  checkAll("Long @@ Multiplication", order.laws[Long @@ Multiplication])
  checkAll("Short @@ Multiplication", order.laws[Short @@ Multiplication])

  checkAll("Boolean @@ Conjunction", monoid.laws[Boolean @@ Conjunction])

  {
    implicit val B = std.anyVal.booleanInstance.conjunction
    checkAll("Boolean", monoid.laws[Boolean])
  }

  checkAll("Short", monoid.laws[Short])
  checkAll("Short @@ Multiplication", monoid.laws[Short @@ Multiplication])
  checkAll("Byte", monoid.laws[Byte])
  checkAll("Byte @@ Multiplication", monoid.laws[Byte @@ Multiplication])
  checkAll("Long", monoid.laws[Long])
  checkAll("Long @@ Multiplication", monoid.laws[Long @@ Multiplication])

  checkAll("Int", group.laws[Int])
  checkAll("Short", group.laws[Short])
  checkAll("Long", group.laws[Long])
}
