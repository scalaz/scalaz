package scalaz
package std
package math

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

object BigIntTest extends SpecLite {
  checkAll("BigInt", enum.laws[BigInt])
  checkAll("BigInt @@ Multiplication", order.laws[BigInt @@ Multiplication])

  checkAll("BigInt @@ Multiplication", monoid.laws[BigInt @@ Multiplication])
  checkAll("BigInt", monoid.laws[BigInt])
}
