package scalaz
package xml

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ContentTest extends Spec {
  checkAll(equal.laws[Content])
}
