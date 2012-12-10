package scalaz

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._

class NeedTest extends testlib.Spec {
  // TODO check distributive laws

  checkAll("Value", monad.laws[Value])
  checkAll("Value", comonad.laws[Value])

  checkAll("Name", monad.laws[Name])
  checkAll("Name", comonad.laws[Name])

  checkAll("Need", monad.laws[Need])
  checkAll("Need", comonad.laws[Need])
}
