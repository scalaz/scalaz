package scalaz
package std
package java
package util
package concurrent

import _root_.java.util.concurrent.Callable
import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._

class CallableTest extends testlib.Spec {
  checkAll("Callable", equal.laws[Callable[Int]])
}
