package scalaz
package effect

import java.io._
import std.effect.closeable._

object ResourceTest extends SpecLite {

  "Resource" should {

    "close the resource properly (1)" in {
      val r = new StringReader("abcdef")
      r.read
      Resource[StringReader].close(r).unsafePerformIO
      try {
        r.read
        fail("should have thrown")
      } catch {
        case ioe: IOException => // ok
      }
    }

    "close the resource properly (2)" in {
      val r = new StringReader("abcdef")
      r.read
      IO(r).using(_ => IO.ioUnit).unsafePerformIO
      try {
        r.read
        fail("should have thrown")
      } catch {
        case ioe: IOException => // ok
      }
    }

  }

}
