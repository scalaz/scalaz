package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object ArrowTest extends SpecLite {

  implicit val eqBooleanInt = new Equal[Boolean => Int] {
    def equal(f1: Boolean => Int, f2: Boolean => Int): Boolean =
      f1(true) == f2(true) && f1(false) == f2(false)
  }
  checkAll(apply.laws[Function1[Boolean, ?]])
}
