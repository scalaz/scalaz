package scalaz

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import Id._

class IdTest extends testlib.Spec {
  checkAll(monad.laws[Id])
  checkAll(traverse.laws[Id])
}
