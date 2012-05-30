package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Gen, Arbitrary}
import Lens.{lens => _, _}
import Id._

class LensTest extends Spec {

  {
    implicit def lensArb = Arbitrary(Gen.value(Lens.lensId[Id, Int]))
    implicit def lensEqual = new Equal[Lens[Int, Int]] {
      def equal(a1: Lens[Int, Int], a2: Lens[Int, Int]): Boolean = a1.get(0) == a2.get(0)
    }
    checkAll("Lens", category.laws[Lens]) // not really testing much!
  }

  checkAll("id", lens.laws(Lens.lensId[Id, Int]))
  checkAll("trivial", lens.laws(Lens.trivialLens[Id, Int]))
  checkAll("codiagLens", lens.laws(Lens.codiagLens[Id, Int]))
  checkAll("Tuple2.first", lens.laws(Lens.firstLens[Int, Int]))
  checkAll("Tuple2.second", lens.laws(Lens.secondLens[Int, Int]))
  checkAll("Set.contains", lens.laws(Lens.lensId[Id, Set[Int]].contains(0)))
  checkAll("Map.member", lens.laws(Lens.lensId[Id, Map[Boolean, Int]].member(true)))
  checkAll("sum", lens.laws(Lens.firstLens[Int, String].sum(Lens.firstLens[Int, String])))

  "NumericLens" ^
    "+=" ! {
      check((i: Int) => (Lens.lensId[Id, Int] += i).run(1) must be_=== ((i + 1) -> (i + 1)))
    }^
    "-=" ! {
      check((i: Int) => (Lens.lensId[Id, Int] -= i).run(1) must be_=== ((1 - i) -> (1 - i)))
    }^
    "*=" ! {
      check((i: Int) => (Lens.lensId[Id, Int] *= i).run(2) must be_=== ((i * 2) -> (i * 2)))
    }
}
