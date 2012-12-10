package scalaz
package concurrent

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import std.AllInstances._

class PromiseTest extends testlib.Spec {
  implicit def promiseEqual[A: Equal] = Equal[A].contramap((_: Promise[A]).get)

  checkAll(monad.laws[Promise])
  checkAll(traverse.laws[Promise])
  checkAll(comonad.laws[Promise])
}