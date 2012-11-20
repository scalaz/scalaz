package scalaz
package std
package math

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

class BigIntTest extends Spec {
  checkAll("BigInt", order.laws[BigInt])
  checkAll("BigInt @@ Multiplication", order.laws[BigInt @@ Multiplication])

  checkAll("BigInt @@ Multiplication", monoid.laws[BigInt @@ Multiplication])
  checkAll("BigInt", ring.laws[BigInt])
}
