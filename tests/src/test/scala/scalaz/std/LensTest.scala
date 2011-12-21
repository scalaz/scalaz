package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class LensTest extends Spec {
//  import Lens._

  checkAll("id", lens.laws(Lens.lensId[Int]))
  checkAll("trivial", lens.laws(Lens.trivialLens[Int]))
  checkAll("codiagLens", lens.laws(Lens.codiagLens[Int]))
  checkAll("Tuple2.first", lens.laws(Lens.firstLens[Int, Int]))
  checkAll("Tuple2.second", lens.laws(Lens.secondLens[Int, Int]))
  checkAll("Set.contains", lens.laws(Lens.lensId[Set[Int]].contains(0)))
}
