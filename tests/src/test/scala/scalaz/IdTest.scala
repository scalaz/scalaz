package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class IdTest extends Spec {
  checkAll(monad.laws[Id])
  checkAll(traverse.laws[Id])
}
