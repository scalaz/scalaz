package scalaz
package effect

import syntax.foldable._
import std.stream._

class IOTest extends Spec {
  "IO" should {
    // as reported in <https://groups.google.com/d/msg/scalaz/BIhItmdejeI/zZ-fSH7ZzfwJ>
    // fix in bb4ebd650
    "not stack overflow" in {
      var counter: Long = 0L
      val action: IO[Unit] = Stream.from(1).take(40000).traverse_(i => IO { counter += i })
      action.unsafePerformIO()
      counter must_== 800020000L
    }
  }
}

// vim: expandtab:ts=2:sw=2
