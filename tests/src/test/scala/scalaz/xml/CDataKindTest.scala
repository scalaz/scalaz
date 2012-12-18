package scalaz
package xml

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class CDataKindTest extends Spec {
  checkAll(equal.laws[CDataKind])
}
