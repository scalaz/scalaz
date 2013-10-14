package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Id._
import org.scalacheck.Prop.forAll

object IdTest extends SpecLite {
  checkAll(monad.laws[Id])
  checkAll(traverse.laws[Id])
  checkAll(comonad.laws[Id])
}
