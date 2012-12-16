package scalaz
package std

import collection.immutable.IndexedSeq

import org.scalacheck.Arbitrary

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary.NonEmptyListArbitrary
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

  "takeWhileM" ! prop {
    (xs: IndexedSeq[Int]) =>
      takeWhileM[Int, Id](xs)(evenp) must be_===(xs takeWhile evenp)
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
}
