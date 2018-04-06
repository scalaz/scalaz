package scalaz

import scalaz.std.anyVal._

object EphemeralStreamTestJVM extends SpecLite {
  "foldLeft large stream" in {
    val list = List.fill(10000000)(1)
    val xs = EphemeralStream(list : _*)
    Foldable[EphemeralStream].foldLeft(xs, 0)(_ + _) must_===(list.sum)
  }

  "reading from a stream in parallel should be safe" in {
    import Scalaz._
    import scalaz.concurrent._
    val limit = 10000000
    val stm = EphemeralStream.range(1, limit + 1).memoized
    val nthreads = 4
    // Ensure that we get back the right numbers, in the right order.
    val tsks = Task.gatherUnordered( IList.fill(nthreads)(Task(stm.foldLeft(0)((prev, n) => { (prev + 1) must_=== n; n }))) )
    // And that the result contains the last number for each of the threads.
    tsks.unsafePerformSync must_=== IList.fill(nthreads)(limit)
  }
}
