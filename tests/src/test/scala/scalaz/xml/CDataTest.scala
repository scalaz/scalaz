package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import CData._
import org.scalacheck.Prop.forAll

object CDataTest extends SpecLite {
  checkAll(equal.laws[CData])
  checkAll(lens.laws(verbatimCDataL))
  checkAll(lens.laws(dataCDataL))
  checkAll(lens.laws(lineCDataL))
}
