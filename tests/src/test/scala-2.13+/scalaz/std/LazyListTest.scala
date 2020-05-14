package scalaz
package std

import std.AllInstances._
import std.lazylist._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop.forAll

object LazyListTest extends SpecLite {
  checkAll(order.laws[LazyList[Int]])
  checkAll(monoid.laws[LazyList[Int]])
  checkAll(monadPlus.strongLaws[LazyList])
  checkAll(bindRec.laws[LazyList])
  checkAll(traverse.laws[LazyList])
  checkAll(cobind.laws[LazyList])
  checkAll(isEmpty.laws[LazyList])
  checkAll(zip.laws[LazyList])
  checkAll(align.laws[LazyList])

  import std.lazylist.lazylistSyntax._
  import syntax.foldable._

  "Order[LazyList[Int]] is consistent with Order[List[Int]]" ! forAll {
    (a: LazyList[Int], b: LazyList[Int]) =>
      Order[LazyList[Int]].order(a, b) must_=== Order[List[Int]].order(a.toList, b.toList)
  }

  "Order[LazyList[Int]] is lazy" ! {
    var evaluated = false
    val a = 1 #:: {evaluated = true; 2} #:: LazyList.empty[Int]
    val b = 0 #:: LazyList.empty[Int]
    Order[LazyList[Int]].order(a, b) must_=== Ordering.GT
    Order[LazyList[Int]].order(b, a) must_=== Ordering.LT
    evaluated must_=== false
  }

  "intercalate empty LazyList is flatten" ! forAll((a: LazyList[LazyList[Int]]) => a.intercalate(LazyList.empty[Int]) must_===(a.flatten))

  "intersperse then remove odd items is identity" ! forAll {
    (a: LazyList[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  "intercalate is same as intersperse(s).flatten" ! forAll {
    (a: LazyList[LazyList[Int]], b: LazyList[Int]) =>
      a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! forAll {
    def intersperse[A](as: LazyList[A], a: A): LazyList[A] = {
      def loop(rest: LazyList[A]): LazyList[A] = rest match {
        case h #:: t => a #:: h #:: loop(t)
        case _ => LazyList.empty
      }
      as match {
        case h #:: t => h #:: loop(t)
        case _ => LazyList.empty
      }
    }
    (a: LazyList[Int], b: Int) => (a.intersperse(b) must_===(intersperse(a, b)))
  }


  "foldl is foldLeft" ! forAll {(rnge: LazyList[List[Int]]) =>
    val F = Foldable[LazyList]
    (rnge.foldLeft(List[Int]())(_++_)
      must_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr is foldRight" ! forAll {(rnge: LazyList[List[Int]]) =>
    val F = Foldable[LazyList]
    (rnge.foldRight(List[Int]())(_++_)
      must_===(F.foldRight(rnge, List[Int]())(_++_)))
  }

  "foldMap evaluates lazily" in {
    Foldable[LazyList].foldMap(LazyList.continually(false))(identity)(booleanInstance.conjunction) must_===(false)
  }

  "foldMap1Opt identity" ! forAll {
    (xs: LazyList[Int]) =>
    Foldable[LazyList].foldMap1Opt(xs)(LazyList(_)).getOrElse(LazyList.empty) must_===(xs)
  }

  "foldMap1Opt evaluates lazily" in {
    Foldable[LazyList].foldMap1Opt(LazyList.continually(false))(identity)(booleanInstance.conjunction) must_===(Some(false))
  }

  "foldRight evaluates lazily" in {
    Foldable[LazyList].foldRight(LazyList.continually(true), true)(_ || _) must_===(true)
  }

  "foldMapLeft1Opt identity" ! forAll {
    (xs: LazyList[Int]) =>
    Foldable[LazyList].foldMapLeft1Opt(xs.reverse)(LazyList(_))((xs, x) => x #:: xs) must_===(
      if (xs.isEmpty) None else Some(xs)
    )
  }

  "foldMapRight1Opt identity" ! forAll {
    (xs: LazyList[Int]) =>
    Foldable[LazyList].foldMapRight1Opt(xs)(LazyList(_))(_ #:: _) must_===(
      if (xs.isEmpty) None else Some(xs)
    )
  }

  "foldMapRight1Opt evaluates lazily" in {
    Foldable[LazyList].foldMapRight1Opt(LazyList.continually(true))(identity)(_ || _) must_===(Some(true))
  }

  "zipL" in {
    val size = 100
    val infinite = LazyList.from(1)
    val finite = LazyList.range(0, size)
    val F = Traverse[LazyList]
    F.zipL(infinite, infinite)
    F.zipL(finite, infinite).length must_===(size)
    F.zipL(finite, infinite) must_===((finite zip infinite).map{x => (x._1, Option(x._2))})
    F.zipL(infinite, finite).take(1000).length must_===(1000)
    F.zipL(infinite, finite).takeWhile(_._2.isDefined).length must_===(size)
  }

  "filter" ! forAll {
    (xs: LazyList[Int], p: Int => Boolean) => MonadPlus[LazyList].filter(xs)(p) must_=== xs.filter(p)
  }

  object instances {
    def equal[A: Equal] = Equal[LazyList[A]]
    def order[A: Order] = Order[LazyList[A]]
    def monoid[A] = Monoid[LazyList[A]]
    def bindRec = BindRec[LazyList]
    def monadPlus = MonadPlus[LazyList]
    def traverse = Traverse[LazyList]
    def zip = Zip[LazyList]
    def unzip = Unzip[LazyList]
    def align = Align[LazyList]
    def isEmpty = IsEmpty[LazyList]
    def cobind = Cobind[LazyList]
  }
}
