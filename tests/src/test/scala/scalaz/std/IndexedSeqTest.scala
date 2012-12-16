package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary.NonEmptyListArbitrary
import Id._
import syntax.std._

class IndexedSeqTest extends Spec {
/*
  {
    import std.indexedSeq._
    checkAll(equal.laws[IndexedSeq[Int]])
    checkAll(monoid.laws[IndexedSeq[Int]])
    checkAll(monadPlus.strongLaws[IndexedSeq])
    checkAll(traverse.laws[IndexedSeq])
    checkAll(isEmpty.laws[IndexedSeq])
  }

  import std.indexedSeq.indexedSeqSyntax._

  private def evenp(x: Int): Boolean = x % 2 == 0

  "filterM" ! prop {
    (xs: IndexedSeq[Int]) => xs.filterM[Id](evenp) == xs.filter(_ % 2 == 0)
  }

  "initz" ! prop {
    (xs: IndexedSeq[Int]) => initz(xs) must be_=== xs.inits.toIndexedSeq
  }

  "tailz" ! prop {
    (xs: IndexedSeq[Int]) => tailz(xs) must be_=== xs.tails.toIndexedSeq
  }

  "takeWhileM" ! prop {
    (xs: IndexedSeq[Int]) =>
      takeWhileM[Int, Id](xs)(evenp) must be_=== xs.takeWhile(evenp)
  }

  "findM" ! prop {
    (xs: IndexedSeq[Int]) =>
      val i = xs.findIndex(evenp)
      type W[A] = Writer[Int, A]
      findM[Int, W](xs)(x => put(evenp(x))(x))
  }
*/
}
