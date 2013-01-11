package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class NeedTest extends Spec {
  // TODO check distributive laws

  checkAll("Value", monad.laws[Value])
  checkAll("Value", comonad.laws[Value])

  checkAll("Name", monad.laws[Name])
  checkAll("Name", comonad.laws[Name])

  checkAll("Need", monad.laws[Need])
  checkAll("Need", comonad.laws[Need])
}
