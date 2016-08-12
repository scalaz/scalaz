package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object DequeueTest extends SpecLite {
  checkAll(monoid.laws[Dequeue[Int]])
  checkAll(isEmpty.laws[Dequeue])
  checkAll(foldable.laws[Dequeue])
  checkAll(plusEmpty.laws[Dequeue])
  checkAll(functor.laws[Dequeue])

  "fromList works" ! forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toStream must_===(l.toStream)
  }

  "reverse twice is id" ! forAll{ (dq: Dequeue[Int]) ⇒
    dq.reverse.reverse.toIList.must_===(dq.toIList)
  }

  "size consistent with source list" ! forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).size must_===(l.size)
  }

  "reverse stream works" ! forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toBackStream must_===(l.reverse.toStream)
  }

  "toIList consistent with Ilist.fromFoldable" ! forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toIList must_===(IList.fromFoldable(l))
  }

  "toBackIList consistent with Ilist.fromFoldable" ! forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toBackIList must_===(IList.fromFoldable(l.reverse))
  }

  "toBackIList.reverse is toIList" ! forAll{ (l: List[Int]) ⇒
    val q = l.foldLeft[Dequeue[Int]](Dequeue.empty)((q,a) ⇒ q cons a)
    q.toBackIList.reverse must_===(q.toIList)
  }

  "snoc works" ! forAll{ (l: List[Int]) ⇒
    (l.foldLeft[Dequeue[Int]](Dequeue.empty)((q,a) ⇒ q snoc a)).toStream must_=== l.toStream
  }

  "cons works" ! forAll{ (l: List[Int]) ⇒
    (l.foldRight[Dequeue[Int]](Dequeue.empty)((a,q) ⇒ q cons a)).toStream must_=== l.toStream
  }
}
