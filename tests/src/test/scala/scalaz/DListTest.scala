package scalaz

import std.AllInstances._
import syntax.equal._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DListTest extends SpecLite {

  checkAll(equal.laws[DList[Int]])
  checkAll(monoid.laws[DList[Int]])
  checkAll(zip.laws[DList])
  checkAll(traverse.laws[DList])
  checkAll(isEmpty.laws[DList])
  checkAll(bindRec.laws[DList])
  checkAll(monadPlus.strongLaws[DList])

  "DList append" ! ((0 to 100000).foldLeft(DList[Int]())(_ :+ _).toList must_== (0 to 100000).toList)

  "headOption, tailOption" ! forAll { (n: Int, d: DList[Int]) =>

    // Defined when appropriate?
    val nonempty = d.uncons(false, (_, _) => true)
    d.headOption.isDefined must_=== nonempty
    d.tailOption.isDefined must_=== nonempty

    // If defined, are values correct?
    val d0 = n +: d
    check(d0.headOption === Some(n)) // no Show instance, can't use must_===
    check(d0.tailOption === Some(d))

  }

  object instances {
    def equal[A: Equal] = Equal[DList[A]]
    def monoid[A] = Monoid[DList[A]]
    def monadPlus = MonadPlus[DList]
    def bindrec = BindRec[DList]
    def traverse = Traverse[DList]
    def zip = Zip[DList]
    def isEmpty = IsEmpty[DList]
  }
}
