package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._

class HeapTest extends Spec {
  checkAll(equal.laws[Heap[Int]])
  checkAll(monoid.laws[Heap[Int]])

  def pred(i: Int) = i % 2 == 0

  "order maintained for toList" ! prop {
    (a: Heap[Int]) => a.toList must be_===(a.toList.sorted)
  }

  "toList / toStream" ! prop {
    (a: Heap[Int]) => a.toStream must be_===(a.toList.toStream)
  }

  "filter" ! prop {
    (a: Heap[Int]) => a.filter(pred).toStream must be_===(a.toStream.filter(pred))
  }

  "partition" ! prop {
    (a: Heap[Int]) =>
      val (ts, fs) = a.partition(pred)
      ts.forall(pred) must be_===(true)
      fs.exists(pred) must be_===(false)
  }

  "split" ! prop {
    (a: Heap[Int], x: Int) =>
      val (lt, eq, gt) = a.split(x)
      lt.forall(_ < x) must be_===(true)
      eq.forall(_ == x) must be_===(true)
      gt.forall(_ > x) must be_===(true)
  }
}
