package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._

object IdTest extends SpecLite {
  checkAll(monad.laws[Id])
  checkAll(traverse.laws[Id])
  checkAll(zip.laws[Id])
  checkAll(align.laws[Id])
  checkAll(comonad.laws[Id])
}
