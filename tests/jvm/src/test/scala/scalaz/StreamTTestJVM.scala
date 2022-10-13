package scalaz

import std.AllInstances._

object StreamTTestJVM extends SpecLite {
  "switch" should {
    "discard previous streams when a new stream is emitted" in {
      import scala.concurrent._, duration._
      implicit def ec: ExecutionContext = ExecutionContext.parasitic

      val stream1 = Promise[StreamT.Step[Future, StreamT[Future, Int]]]()
      val stream2 = Promise[StreamT.Step[Future, StreamT[Future, Int]]]()
      val slow = Promise[StreamT.Step[Future, Int]]()
      val result = StreamT.switch(StreamT(stream1.future)).toLazyList

      stream1.success(StreamT.Yield(1 :: 2 :: StreamT(slow.future), StreamT(stream2.future)))
      stream2.success(StreamT.Yield(10 :: 20 :: 30 :: StreamT.empty[Future, Int], StreamT.empty[Future, StreamT[Future, Int]]))
      slow.success(StreamT.Yield(3, StreamT.empty))
      Await.result(result, 1.seconds) must_=== LazyList(1, 2, 10, 20, 30)
    }
  }

}
