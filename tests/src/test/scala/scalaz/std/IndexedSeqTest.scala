package scalaz
package std

import collection.immutable.IndexedSeq

import org.scalacheck.Arbitrary

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._
import syntax.std._

class IndexedSeqTest extends Spec {
  implicit def indexedSeqArb[A: Arbitrary] =
    Arbitrary(implicitly[Arbitrary[List[A]]].arbitrary map (_.toIndexedSeq))

  import std.indexedSeq._
  checkAll(equal.laws[IndexedSeq[Int]])
  checkAll(monoid.laws[IndexedSeq[Int]])
  checkAll(monadPlus.strongLaws[IndexedSeq])
  checkAll(traverse.laws[IndexedSeq])
  checkAll(isEmpty.laws[IndexedSeq])

  import std.indexedSeq.indexedSeqSyntax._
  import syntax.index._

  private def evenp(x: Int): Boolean = x % 2 == 0

  "filterM" ! prop {
    (xs: IndexedSeq[Int]) => xs.filterM[Id](evenp) == xs.filter(_ % 2 == 0)
  }

  "initz" ! prop {
    (xs: IndexedSeq[Int]) =>
      initz(xs) must be_===(xs.inits.toIndexedSeq.reverse)
  }

  "tailz" ! prop {
    (xs: IndexedSeq[Int]) => tailz(xs) must be_===(xs.tails.toIndexedSeq)
  }

  "spanM" ! prop {
    (xs: IndexedSeq[Int]) =>
      (xs.spanM[Id](evenp)
       must be_===(xs.takeWhile(evenp) -> xs.dropWhile(evenp)))
  }

  "takeWhileM" ! prop {
    (xs: IndexedSeq[Int]) =>
      takeWhileM[Int, Id](xs)(evenp) must be_===(xs takeWhile evenp)
  }

  "groupWhen" ! prop {
    (xs: IndexedSeq[Int]) =>
      (xs.groupWhen(_ < _)
       must be_===(list.groupWhen(xs.toList)(_ < _)
                   .map(_.toIndexedSeq).toIndexedSeq))
  }

  "partitionM" ! prop {
    (xs: IndexedSeq[Int]) =>
      val (evens, odds) = xs.partitionM[Id](evenp)
      (evens.toSet & odds.toSet) must be_===(Set[Int]())
      (evens.filter(evenp) ++
       odds.filter(i => !evenp(i))).toSet must be_===(xs.toSet)
  }

  "findM" ! prop {
    (xs: IndexedSeq[Int]) =>
      val i = xs indexWhere evenp
      type W[A] = Writer[IndexedSeq[Int], A]
      val wxs = findM[Int, W](xs)(x =>
        WriterT.writer(IndexedSeq(x) -> evenp(x)))
      (wxs.written, wxs.value) must be_==={
        if (i < 0) (xs, None)
        else (xs take (i+1), Some(xs(i)))
      }
  }

  "mapAccumLeft" ! prop {
    (xs: IndexedSeq[Int]) =>
      mapAccumLeft(xs)(IndexedSeq[Int](), (c: IndexedSeq[Int], a) =>
        (c :+ a, a)) must be_===(xs, xs)
  }

  "mapAccumRight" ! prop {
    (xs: IndexedSeq[Int]) =>
      mapAccumRight(xs)(IndexedSeq[Int](), (c: IndexedSeq[Int], a) =>
        (c :+ a, a)) must be_===(xs.reverse, xs)
  }

  "Issue #266" in {
    import syntax.std.list._
    List(1, 2, 4).groupWhen((i1, i2) => scala.math.abs(i1 - i2) <= 1).length must be_===(2)
    List(1, 2, 4).toIndexedSeq.groupWhen((i1, i2) => scala.math.abs(i1 - i2) <= 1).length must be_===(2)
  }

  "index" ! prop { (xs: IndexedSeq[Int], n: Int) =>
    (xs index n) must be_===(if (n >= 0 && xs.size > n) Some(xs(n)) else None)
  }
}
