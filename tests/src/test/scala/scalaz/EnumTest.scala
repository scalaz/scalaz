package scalaz

import scalaz.std.AllInstances._
import scalaz.syntax.`enum`._

object EnumTest extends SpecLite {

  "|--> with step 1" in {
    val expected = IList[Byte](120, 121, 122, 123, 124, 125, 126, 127)
    val generated = 120.toByte |--> (1, 127.toByte)
    assert(generated === expected)
  }

  "|--> with step 2" in {
    assert((120.toByte |--> (2, 126.toByte)) === IList[Byte](120, 122, 124, 126))
  }

  "|--> with step -2" in {
    val expected = IList[Byte](-120, -122, -124, -126)
    val generated = -120.toByte |--> (-2, -126.toByte)
    assert(generated === expected)
  }

  "|--> with step 4" in {
    assert((120.toByte |--> (4, 127.toByte)) === IList[Byte](120, 124))
  }

  "|--> with step -8" in {
    assert((-120.toByte |--> (-8, -128.toByte)) === IList[Byte](-120, -128))
  }
}
