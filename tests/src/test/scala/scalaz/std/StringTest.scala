package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

object StringTest extends SpecLite {
  checkAll(monoid.laws[String])
  checkAll(isEmpty.laws[λ[α => String]])

  checkAll(order.laws[String].withProp("benchmark", order.scalaOrdering[String]))

  "parseBoolean" in {
    import string.parseBoolean
    implicit val s = Show.showFromToString[IllegalArgumentException]
    implicit val e = Equal.equalA[IllegalArgumentException]
    parseBoolean("true") must_===(Validation.success(true))
    parseBoolean("false") must_===(Validation.success(false))
    parseBoolean("1").isSuccess must_===(false)
  }
}
