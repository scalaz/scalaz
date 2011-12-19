package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class NeedTest extends Spec {
  checkAll("Value", monad.laws[Value])
  checkAll("Name", monad.laws[Name])
  checkAll("Need", monad.laws[Need])
}
