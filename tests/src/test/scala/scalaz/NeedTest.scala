package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object NeedTest extends SpecLite {
  // TODO check distributive laws

  checkAll("Value", bindRec.laws[Value])
  checkAll("Value", monad.laws[Value])
  checkAll("Value", comonad.laws[Value])
  checkAll("Value", traverse1.laws[Value])
  checkAll("Value", zip.laws[Value])
  checkAll("Value", align.laws[Value])

  checkAll("Name", bindRec.laws[Name])
  checkAll("Name", monad.laws[Name])
  checkAll("Name", comonad.laws[Name])
  checkAll("Name", traverse1.laws[Name])
  checkAll("Name", zip.laws[Name])
  checkAll("Name", align.laws[Name])

  checkAll("Need", bindRec.laws[Need])
  checkAll("Need", monad.laws[Need])
  checkAll("Need", comonad.laws[Need])
  checkAll("Need", traverse1.laws[Need])
  checkAll("Need", zip.laws[Need])
  checkAll("Need", align.laws[Need])
}
