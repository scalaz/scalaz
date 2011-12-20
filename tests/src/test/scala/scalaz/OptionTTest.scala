package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class OptionTTest extends Spec {

  type OptionTList[A] = OptionT[List, A]

  checkAll(equal.laws[OptionTList[Int]])
  checkAll(monad.laws[OptionTList])
  checkAll(traverse.laws[OptionTList])
}
