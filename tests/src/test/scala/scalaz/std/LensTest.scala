package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Prop, Gen, Arbitrary}
import Lens.{lens => _, _}
import org.scalacheck.Prop.forAll

object LensTest extends SpecLite {

  {
    implicit def lensArb = Arbitrary(Gen.const(Lens.lensId[Int]))
    implicit def lensEqual = new Equal[Lens[Int, Int]] {
      def equal(a1: Lens[Int, Int], a2: Lens[Int, Int]): Boolean = a1.get(0) == a2.get(0)
    }
    checkAll("Lens", category.laws[Lens]) // not really testing much!
  }

  checkAll("id", lens.laws(Lens.lensId[Int]))
  checkAll("trivial", lens.laws(Lens.trivialLens[Int]))
  checkAll("codiagLens", lens.laws(Lens.codiagLens[Int]))
  checkAll("Tuple2.first", lens.laws(Lens.firstLens[Int, Int]))
  checkAll("Tuple2.second", lens.laws(Lens.secondLens[Int, Int]))
  checkAll("Set.contains", lens.laws(Lens.lensId[Set[Int]].contains(0)))
  checkAll("Map.member", lens.laws(Lens.lensId[Map[Boolean, Int]].member(true)))
  checkAll("sum", lens.laws(Lens.firstLens[Int, String].sum(Lens.firstLens[Int, String])))

  "NumericLens" should {
    "+=" ! forAll((i: Int) => (Lens.lensId[Int] += i).run(1) must_=== ((i + 1) -> (i + 1)))
    "-=" ! forAll((i: Int) => (Lens.lensId[Int] -= i).run(1) must_=== ((1 - i) -> (1 - i)))
    "*=" ! forAll((i: Int) => (Lens.lensId[Int] *= i).run(2) must_=== ((i * 2) -> (i * 2)))
  }
}
