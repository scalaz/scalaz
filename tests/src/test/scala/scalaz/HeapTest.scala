package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._
import org.scalacheck.Prop.forAll

object HeapTest extends SpecLite {
  checkAll(equal.laws[Heap[Int]])
  checkAll(monoid.laws[Heap[Int]])
  checkAll(foldable.laws[Heap])

  def pred(i: Int) = i % 2 == 0

  "order maintained for toList" ! forAll {
    (a: Heap[Int]) => a.toList must_===(a.toList.sorted)
  }

  "toList / toStream" ! forAll {
    (a: Heap[Int]) => a.toStream must_===(a.toList.toStream)
  }

  "filter" ! forAll {
    (a: Heap[Int]) => a.filter(pred).toStream must_===(a.toStream.filter(pred))
  }

  "partition" ! forAll {
    (a: Heap[Int]) =>
      val (ts, fs) = a.partition(pred)
      ts.forall(pred) must_===(true)
      fs.exists(pred) must_===(false)
  }

  "split" ! forAll {
    (a: Heap[Int], x: Int) =>
      val (lt, eq, gt) = a.split(x)
      lt.forall(_ < x) must_===(true)
      eq.forall(_ == x) must_===(true)
      gt.forall(_ > x) must_===(true)
  }
}
