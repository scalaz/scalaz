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
  checkAll(isEmpty.laws[LazyOption])
  checkAll(monoid.laws[LazyOption[Int]])

  "monoid" ! forAll { (a: LazyOption[Int], b: LazyOption[Int]) =>
    Monoid[LazyOption[Int]].append(a, b).toOption must_=== Monoid[Option[Int]].append(a.toOption, b.toOption)
  }

  object instances {
    def equal[A: Equal] = Equal[LazyOption[A]]
    def monadPlus = MonadPlus[LazyOption]
    def bindrec = BindRec[LazyOption]
    def cobind = Cobind[LazyOption]
    def traverse = Traverse[LazyOption]
    def zip = Zip[LazyOption]
    def align = Align[LazyOption]
    def isEmpty = IsEmpty[LazyOption]
    def monoid[A: Semigroup] = Monoid[LazyOption[A]]
  }
}
