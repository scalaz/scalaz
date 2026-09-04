package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object UnwriterTTest2 extends SpecLite {

  checkAll(comonad.laws[Unwriter[Int, *]])

}
