package scalaz
package std
package math

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import Tags._

class BigIntTest extends testlib.Spec {
  checkAll("BigInt", order.laws[BigInt])
  checkAll("BigInt @@ Multiplication", order.laws[BigInt @@ Multiplication])

  checkAll("BigInt @@ Multiplication", monoid.laws[BigInt @@ Multiplication])
  checkAll("BigInt", group.laws[BigInt])
}
