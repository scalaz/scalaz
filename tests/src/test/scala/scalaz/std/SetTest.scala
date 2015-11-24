package scalaz
package std

import syntax.foldable._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

object SetTest extends SpecLite {

  checkAll(order.laws[Set[Int]])
  checkAll(monoid.laws[Set[Int]])
  checkAll(isEmpty.laws[Set])
  checkAll(foldable.laws[Set])
  checkAll(FoldableTests.anyAndAllLazy[Set])

  "foldLeftM" should {

    // as reported in <https://github.com/scalaz/scalaz/issues/866>
    // fixed in b0b80be
    "not stack overflow" in {

      def sum = Set(1,2,3).foldLeftM[Option,Int](0) { case (x,y) => Some(x+y) }

      sum must_== Some(6)

    }

  }

}
