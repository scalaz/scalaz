package scalaz
package xml
package cursor

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class HistoryTest extends Spec {
  checkAll(equal.laws[History])
}
