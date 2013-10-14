package scalaz
package xml

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object CDataKindTest extends SpecLite {
  checkAll(equal.laws[CDataKind])
}
