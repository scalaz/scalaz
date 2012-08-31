package scalaz
package std
package java
package math

import _root_.java.math.BigInteger
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

class BigIntegerTest extends Spec {
  checkAll("BigInteger", order.laws[BigInteger])
  checkAll("BigInteger @@ Multiplication", order.laws[BigInteger @@ Multiplication])
  checkAll("BigInteger", ring.laws[BigInteger])
  checkAll("BigInteger", monoid.laws[BigInteger])
  checkAll("BigInteger @@ Multiplication", monoid.laws[BigInteger @@ Multiplication])
}
