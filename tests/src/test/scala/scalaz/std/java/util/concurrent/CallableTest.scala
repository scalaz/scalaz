package scalaz
package std
package java
package util
package concurrent

import _root_.java.util.concurrent.Callable
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object CallableTest extends SpecLite {
  checkAll("Callable", order.laws[Callable[Int]])
  checkAll("Callable", monad.laws[Callable])
}
