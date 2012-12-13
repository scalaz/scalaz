package scalaz
package std

import std.AllInstances._
import scalaz.testlib.ScalazProperties._

class StringTest extends testlib.Spec {
  checkAll(monoid.laws[String])

  checkAll(order.laws[String].withProp("benchmark", order.scalaOrdering[String]))

  "parseBoolean" in {
    import string.parseBoolean
    implicit val s = Show.showFromToString[IllegalArgumentException]
    implicit val e = Equal.equalA[IllegalArgumentException]
    parseBoolean("true") must be_===(Validation.success(true))
    parseBoolean("false") must be_===(Validation.success(false))
    parseBoolean("1").isSuccess must be_===(false)
  }
}
