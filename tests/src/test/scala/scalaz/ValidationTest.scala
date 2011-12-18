package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ValidationTest extends Spec {
  import std.AllInstances._
  type ValidationInt[A] = Validation[Int, A]

  checkAll("Validation", plus.laws[ValidationInt])
  checkAll("Validation", applicative.laws[ValidationInt])
  checkAll("Validation", traverse.laws[ValidationInt])
}
