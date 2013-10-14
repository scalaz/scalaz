package scalaz
package xml
package cursor

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object OpTest extends SpecLite {
  checkAll(equal.laws[Op])
}
