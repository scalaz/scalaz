package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object WriterTTest2 extends SpecLite {

  checkAll(comonad.laws[Writer[Int, *]])

}
