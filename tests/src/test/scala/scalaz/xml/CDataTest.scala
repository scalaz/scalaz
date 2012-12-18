package scalaz
package xml

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import CData._

class CDataTest extends Spec {
  checkAll(equal.laws[CData])
  checkAll(lens.laws(verbatimCDataL))
  checkAll(lens.laws(dataCDataL))
  checkAll(lens.laws(lineCDataL))
}
