package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object LazyOptionTest extends SpecLite {
  checkAll(equal.laws[LazyOption[Int]])
  checkAll(monadPlus.strongLaws[LazyOption])
  checkAll(cobind.laws[LazyOption])
  checkAll(traverse.laws[LazyOption])
  checkAll(zip.laws[LazyOption])
  checkAll(align.laws[LazyOption])
  checkAll(monoid.laws[LazyOption[Int]])

  "monoid" ! forAll { (a: LazyOption[Int], b: LazyOption[Int]) =>
    Monoid[LazyOption[Int]].append(a, b).toOption must_=== Monoid[Option[Int]].append(a.toOption, b.toOption)
  }
}
