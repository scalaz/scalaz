package scalaz
package effect

import syntax.foldable._
import std.stream._

object IOTest extends SpecLite {
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

  "Catchable[IO]" should {

    val C = Catchable[IO]
    val err = new Error("oh noes")
    val bad = C.fail[Int](err)

    "throw exceptions captured via fail()" in {
      try {
        bad.unsafePerformIO
        fail("should have thrown")
      } catch {
        case t: Throwable => t must_== err
      }
    }

    "catch exceptions captured via fail()" in {
      C.attempt(bad).unsafePerformIO must_== -\/(err)
    }

    "catch ambient exceptions" in {
      C.attempt(IO(throw err)).unsafePerformIO must_== -\/(err)
    }

    "properly handle success" in {
      C.attempt(IO(3)).unsafePerformIO must_== \/-(3)
    }

  }

}

// vim: expandtab:ts=2:sw=2
