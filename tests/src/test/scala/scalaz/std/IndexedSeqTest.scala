package scalaz
package std

import collection.immutable.IndexedSeq

import org.scalacheck.Arbitrary

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._
import syntax.std._
import org.scalacheck.Prop.forAll

object IndexedSeqTest extends SpecLite {
  implicit def indexedSeqArb[A: Arbitrary] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary map (_.toIndexedSeq))

  import std.indexedSeq._
  checkAll(equal.laws[IndexedSeq[Int]])
  checkAll(monoid.laws[IndexedSeq[Int]])
  checkAll(monadPlus.strongLaws[IndexedSeq])
  checkAll(traverse.laws[IndexedSeq])
  checkAll(zip.laws[IndexedSeq])
  checkAll(isEmpty.laws[IndexedSeq])
  checkAll(align.laws[IndexedSeq])

  import std.indexedSeq.indexedSeqSyntax._
  import syntax.foldable._

  private def evenp(x: Int): Boolean = x % 2 == 0

  "filterM" ! forAll {
    (xs: IndexedSeq[Int]) => xs.filterM[Id](evenp) == xs.filter(_ % 2 == 0)
  }

  "initz" ! forAll {
    (xs: IndexedSeq[Int]) =>
      initz(xs) must_===(xs.inits.toIndexedSeq.reverse)
  }

  "tailz" ! forAll {
    (xs: IndexedSeq[Int]) => tailz(xs) must_===(xs.tails.toIndexedSeq)
  }

  "spanM" ! forAll {
    (xs: IndexedSeq[Int]) =>
      (xs.spanM[Id](evenp)
       must_===(xs.takeWhile(evenp) -> xs.dropWhile(evenp)))
  }

  "takeWhileM" ! forAll {
    (xs: IndexedSeq[Int]) =>
      takeWhileM[Int, Id](xs)(evenp) must_===(xs takeWhile evenp)
  }

  "groupWhen" ! forAll {
    (xs: IndexedSeq[Int]) =>
      (xs.groupWhen(_ < _)
       must_===(list.groupWhen(xs.toList)(_ < _)
                   .map(_.toIndexedSeq).toIndexedSeq))
  }

  "partitionM" ! forAll {
    (xs: IndexedSeq[Int]) =>
      val (evens, odds) = xs.partitionM[Id](evenp)
      (evens.toSet & odds.toSet) must_===(Set[Int]())
      (evens.filter(evenp) ++
       odds.filter(i => !evenp(i))).toSet must_===(xs.toSet)
  }

  "findM" ! forAll {
    (xs: IndexedSeq[Int]) =>
      val i = xs indexWhere evenp
      type W[A] = Writer[IndexedSeq[Int], A]
      val wxs = findM[Int, W](xs)(x =>
        WriterT.writer(IndexedSeq(x) -> evenp(x)))
      (wxs.written, wxs.value) must_==={
        if (i < 0) (xs, None)
        else (xs take (i+1), Some(xs(i)))
      }
  }

  "mapAccumLeft" ! forAll {
    (xs: IndexedSeq[Int]) =>
      mapAccumLeft(xs)(IndexedSeq[Int](), (c: IndexedSeq[Int], a) =>
        (c :+ a, a)) must_===(xs, xs)
  }

  "mapAccumRight" ! forAll {
    (xs: IndexedSeq[Int]) =>
      mapAccumRight(xs)(IndexedSeq[Int](), (c: IndexedSeq[Int], a) =>
        (c :+ a, a)) must_===(xs.reverse, xs)
  }

  "Issue #266" in {
    import syntax.std.list._
    List(1, 2, 4).groupWhen((i1, i2) => scala.math.abs(i1 - i2) <= 1).length must_===(2)
    List(1, 2, 4).toIndexedSeq.groupWhen((i1, i2) => scala.math.abs(i1 - i2) <= 1).length must_===(2)
  }

  "index" ! forAll { (xs: IndexedSeq[Int], n: Int) =>
    (xs index n) must_===(if (n >= 0 && xs.size > n) Some(xs(n)) else None)
  }

  "groupWhen is groupWhenM[Id]" ! forAll { xs: IndexedSeq[Int] =>
    val f: (Int, Int) => Boolean = _ > _
    xs.groupWhen(f) must_=== xs.groupWhenM[Id.Id](f)
  }
}
