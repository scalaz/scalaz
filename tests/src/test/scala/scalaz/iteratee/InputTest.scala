package scalaz
package iteratee

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class InputTest extends Spec {

  checkAll(equal.laws[Input[Int]])
  checkAll(semigroup.laws[Input[Int]])
  checkAll(monad.laws[Input])
  checkAll(plus.laws[Input])
  checkAll(traverse.laws[Input])

}
