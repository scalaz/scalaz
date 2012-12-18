package scalaz
package xml
package cursor

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class OpTest extends Spec {
  checkAll(equal.laws[Op])
}
